
#dependencies
library(tidyverse)
library(sf)
library(haven)
library(biscale)
library(cowplot)

#working directory = repo base dir
setwd("R:/Project/natrent-city-sub")

#load in tract extract geojson
tract <- read_sf("./output/extract/tract_listing_count_thru_2019.geojson")

#load in tract panel
clust <- read_sf("./input/us-hclust-zscore.shp")


#### A. Join up ---------------------------------------------------------------

#turn tract_clust into clust level polygons for each CBSA
clust <- clust %>%
  select(clust, geometry) %>%
  st_transform(crs = 102008)
  
#make sure both are projected to same CRS (equal area conic)
tract_centroid <- tract %>%
  select(trt_id, geometry) %>%
  st_transform(crs = 102008) %>%
  st_centroid()

#point in polygon intersection
tract_clust <- st_join(tract_centroid, clust)

#mutate a common name key for join, reduce table to necessary columns
tract_clust <- tract_clust %>%
  st_drop_geometry()

#join data to sf
tract <- inner_join(tract, tract_clust) %>%
  st_as_sf() 

top100 <- tract %>%
  st_drop_geometry() %>%
  distinct(met_id, met_tot_pop) %>%
  top_n(100, met_tot_pop) %>%
  pull(met_id)

tract <- tract %>%
  filter(met_id %in% top100)

#metros where clusters do not cover 100%
#tract %>% 
#  st_drop_geometry() %>% 
#  group_by(met_id, cbsa) %>% 
#  summarize(n_na = sum(is.na(clust)),
#            shr = sum(is.na(clust))/n()) %>% 
#  filter(shr > 0) %>%
#  arrange(desc(shr)) %>%
#  as.data.frame()


#### B. Construct lambda ------------------------------------------------------

#make variables for boeing replication
tract <- tract %>% 
  mutate(vac_hu_ratio = (trt_tot_vac_hu_for_rent+1)/(met_tot_vac_hu_for_rent+1)) %>% 
  group_by(met_id) %>% 
  mutate(met_tot_cl_listings = sum(cl_listing_count),
         met_tot_apts_listings = sum(apts_listing_count),
         count_tracts = n()) %>% 
  ungroup() %>% 
  mutate(expected_cl_listings = met_tot_cl_listings*vac_hu_ratio,
         expected_apts_listings = met_tot_apts_listings*vac_hu_ratio,
         cl_lambda = cl_listing_count/expected_cl_listings,
         apts_lambda = apts_listing_count/expected_apts_listings) 


#### C. Map spatial distribution of lambda  -----------------------------------

#first, reproject the tract sf to better output CRS (web mercator)
tract <- st_transform(tract, 3857)

#now save a map theme helper fn
theme_map <- function(...) {
  default_font_color = "Black"
  default_background_color = "White"
  
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      
      legend.position = "bottom",
      legend.key.width = unit(.75, "inch"),
      legend.key.height = unit(.2, "inch"), 
      legend.title = element_text(hjust = -.75, vjust = .95),
      legend.text = element_text(angle = 45, hjust = 1, vjust = 1),
      
      ...
    )
}

#vector of 100 cbsas
metros <- unique(tract$cbsa) %>% sort()

#Univariate choro: save a function that takes a cbsa code and maps its
#lambdas for CL and apartments.com
choro_ratios <- function(metro){
  cbsa_tracts <- tract %>%
    filter(cbsa == metro) 
  
  cbsa_tracts <- cbsa_tracts %>%
    select(cbsa, ends_with("lambda")) %>%
    gather(key = "measure", value = "value", -cbsa, -geometry) %>%
    mutate(measure = ifelse(measure == "cl_lambda", "Craigslist", measure),
           measure = ifelse(measure == "apts_lambda", "Apartments.com", measure),
           trunc_value = ifelse(value > 2, 2, value))
  
  ggplot(cbsa_tracts, aes(fill = trunc_value)) +
    facet_grid(~ measure) +
    geom_sf(lwd = 0.0001, color = NA) +
    scale_fill_gradientn(colours = c(scales::muted("red"), "white", scales::muted("blue")), 
                         labels = c("Under", "", "Parity", "", "Over"),
                         breaks = c(0, .5, 1, 1.5, 2),
                         values = scales::rescale(c(0, 1, 2)),
                         guide = "colorbar", 
                         limits= c(0, 2)) +
    theme_map() +
    labs(fill = "Truncated Lambda") +
    ggsave(filename = paste0("./output/choro/lambda/", 
                             str_split_fixed(metro, "-|,|/", n = 2)[1],
                             "_lambda.pdf"),
           width = 12, height = 8, dpi = 300) 
}

#produce the chroropleth for each metropolitan area by mapping
#all unique metro codes to choro_ratio
map(metros, choro_ratios)

#Bivariate choro: save another function, this time for bivariate
#map of lambda (discrete cats) by listing source
choro_bivar <- function(metro){
 
  #a little bit of data preparation
  cbsa_tracts <- tract %>%
    filter(cbsa == metro) %>%
    mutate(under_cl = 1 - ifelse(cl_lambda > 1, 1, cl_lambda),
           under_apts = 1 - ifelse(apts_lambda > 1, 1, apts_lambda))
  
  #add the biclass col using fn from biscale lib
  under_tracts <- bi_class(cbsa_tracts, 
                          x = under_cl, y = under_apts,
                          style = "equal", dim = 3)

  #make the legends
  under_legend <- bi_legend(pal = "DkCyan",
                      dim = 3,
                      xlab = "Craigslist",
                      ylab = "Apts.com",
                      size = 8) +
    theme(legend.text = element_text(family = "Helvetica"))
  
  #make the under map
  under_choro <- ggplot() +
    geom_sf(data = under_tracts, aes(fill = bi_class), 
            color = NA, size = 0.1, show.legend = FALSE) +
    bi_scale_fill(pal = "DkCyan", dim = 3) +
    labs(title = paste("Underrepresented Neighborhoods in", metro)) +
    bi_theme() +
    theme(plot.title = element_text(size = 14, family = "Helvetica"))
  
  #assemble the map and legend
  under_gg <- ggdraw() +
    draw_plot(under_choro, 0, 0, 1, 1) +
    draw_plot(under_legend, 0.025, 0.025, 0.2, 0.2)
  
  #save to disk
  save_plot(filename = paste0("./output/choro/bivar/", 
                              str_split_fixed(metro, "-|,|/", n = 2)[1],
                              "_bivar.pdf"),
            plot = under_gg,
            base_height = 8, base_asp = 1.25) 
  }

#produce the chroropleth for each metropolitan area by mapping
#all unique metro codes to choro_bivar
map(metros, choro_bivar)


#### D. Map median rent spatial distribution -----------------------------------

choro_med_rent <- function(metro){
  
  #a little bit of data preparation
  cbsa_tracts <- tract %>%
    filter(cbsa == metro) 
  
  #pivot longer so we can facet by listing source
  cbsa_tracts <- cbsa_tracts %>%
    select(cbsa, ends_with("median")) %>%
    gather(key = "measure", value = "value", -cbsa, -geometry) %>%
    mutate(measure = ifelse(measure == "cl_median", "Craigslist", measure),
           measure = ifelse(measure == "apts_median", "Apartments.com", measure))
  
  #make the map for metro, save to disk
  ggplot(cbsa_tracts, aes(fill = value)) +
    facet_grid(~ measure) +
    geom_sf(lwd = 0.0001, color = NA) +
    scale_fill_viridis_c(labels = scales::dollar) +
    theme_map() +
    labs(fill = "Median Rent") +
    ggsave(filename = paste0("./output/choro/med rent/", 
                             str_split_fixed(metro, "-|,|/", n = 2)[1],
                             "_med_rent.pdf"),
           width = 12, height = 8, dpi = 300) 
}

map(metros, choro_med_rent)

choro_med_rent_1B <- function(metro){
  
  #a little bit of data preparation
  cbsa_tracts <- tract %>%
    filter(cbsa == metro) 
  
  #pivot longer so we can facet by listing source
  cbsa_tracts <- cbsa_tracts %>%
    select(cbsa, ends_with("median_1B")) %>%
    gather(key = "measure", value = "value", -cbsa, -geometry) %>%
    mutate(measure = ifelse(measure == "cl_median_1b", "Craigslist", measure),
           measure = ifelse(measure == "apts_median_1b", "Apartments.com", measure))
  
  #make the map for metro, save to disk
  ggplot(cbsa_tracts, aes(fill = value)) +
    facet_grid(~ measure) +
    geom_sf(lwd = 0.0001, color = NA) +
    scale_fill_viridis_c(labels = scales::dollar) +
    theme_map() +
    labs(fill = "Median Rent") +
    ggsave(filename = paste0("./output/choro/med rent 1B/", 
                             str_split_fixed(metro, "-|,|/", n = 2)[1],
                             "_med_rent_1B.pdf"),
           width = 12, height = 8, dpi = 300) 
}

map(metros, choro_med_rent_1B)


#### E. Pivot data longer by platform -----------------------------------------

cl_mdata <- tract %>%
  select(everything(), -apts_lambda) %>%
  mutate(platform = "Craigslist") %>%
  rename(lambda = cl_lambda,
         med_rent = cl_median)

apts_mdata <- tract %>%
  select(everything(), -cl_lambda) %>%
  mutate(platform = "Apartments.com") %>%
  rename(lambda = apts_lambda,
         med_rent = apts_median)

tract_mdata <- bind_rows(cl_mdata, apts_mdata) %>%
  group_by(met_id) %>%
  mutate(dis_blk_wht = (.5) * sum(abs(trt_tot_blk/sum(trt_tot_blk) - 
                                        trt_tot_wht/sum(trt_tot_wht)))) %>%
  ungroup()


#### F. Correlation of rents between platforms --------------------------------

tract_mdata %>%
  st_drop_geometry() %>%
  filter(!is.na(clust)) %>%
  group_by(platform, clust) %>%
  summarize(mean_diff = mean(med_rent - trt_med_gross_rent, na.rm = T),
            mean_pct_diff = mean((med_rent - trt_med_gross_rent)/trt_med_gross_rent, na.rm = T),
            correlation = cor(med_rent, trt_med_gross_rent, use = "pairwise.complete.obs"))

tract_mdata %>%
  st_drop_geometry() %>%
  group_by(met_name) %>%
  summarize(correlation = cor(med_rent, trt_med_gross_rent, use = "pairwise.complete.obs")) %>%
  arrange(desc(correlation)) %>%
  as.data.frame()

tract_mdata %>%
  st_drop_geometry() %>%
  group_by(platform, met_name) %>%
  summarize(correlation = cor(med_rent, trt_med_gross_rent, use = "pairwise.complete.obs")) %>%
  arrange(desc(correlation)) %>%
  as.data.frame()


#### G. Save data for modeling -------------------------------------------------

#save data for models
save(tract_mdata, file = "./output/mdata/tract_mdata.RData")

