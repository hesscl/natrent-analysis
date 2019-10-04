
#dependencies
library(tidyverse)
library(sf)
library(haven)

#load in tract extract geojson
tract <- st_read("./output/extract/tract_listing_count_thru_sept.geojson")

#load in tract panel
tract_clust <- read_csv("./input/tract_clust.csv")


#### A. Join up ---------------------------------------------------------------

#mutate a common name key for join, reduce table to necessary columns
tract_clust <- tract_clust %>%
  mutate(trt_id = GISJOIN,
         clust = as.character(clust)) %>%
  distinct(trt_id, clust)

#check coverage, 2.8% of tracts w/o match in tract extract from natrent
#probably should re-run cluster analysis based on ACS shp + estimates
tract %>% 
  filter(trt_id %in% tract_clust$trt_id) %>%
  pull(trt_id) %>%
  length(.) / nrow(tract)

#join data to sf
tract <- left_join(tract, tract_clust) %>%
  st_as_sf() 


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


#### C. Look at spatial distribution of lambda and sociodemographics ----------

#first, reproject the tract sf to better output CRS (pseudo mercator)
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
metros <- unique(tract$cbsa)

#now save a function to pass to map with the vector of CBSAs
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
    geom_sf(lwd = 0.0001, color = "grey60") +
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

map(metros, choro_ratios)





