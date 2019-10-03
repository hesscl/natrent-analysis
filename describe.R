
#dependencies
library(tidyverse)
library(sf)
library(haven)

#load in tract extract geojson
tract <- st_read("./output/extract/tract_listing_count_thru_sept.geojson")

#load in tract panel
tract_panel <- read_dta("../dissertation/metro-povtrends/panel_for_stata.dta")

#check coverage, little less than 3% of tracts w/o match in tract extract from natrent
tract %>% 
  filter(trt_id %in% tract_panel$GISJOIN) %>%
  pull(trt_id) %>%
  length(.) / nrow(tract)


#### A. Join up ---------------------------------------------------------------

#mutate a common name key for join, reduce table to necessary columns
tract_panel <- tract_panel %>%
  mutate(trt_id = GISJOIN) %>%
  distinct(trt_id, clust)

#join data to sf
tract <- left_join(tract, tract_panel) %>%
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
      legend.title = element_text(hjust = -.75, vjust = .75),
      
      ...
    )
}

#vector of 100 cbsas
metros <- unique(tract$cbsa)

#now save a function to pass to map with the vector of CBSAs
choro_ratios <- function(metro){
  cbsa_tracts <- tract %>%
    filter(cbsa == metro) 
  
  cl_bc <- bestNormalize::boxcox(cbsa_tracts$cl_lambda + 1)
  apts_bc <- bestNormalize::boxcox(cbsa_tracts$apts_lambda + 1)
  
  cbsa_tracts$cl_lambda_bc <- predict(cl_bc)
  cbsa_tracts$apts_lambda_bc <- predict(apts_bc)
  
  cbsa_tracts <- cbsa_tracts %>%
    select(cbsa, ends_with("lambda_bc")) %>%
    gather(key = "measure", value = "value", -cbsa, -geometry)
  
  ggplot(cbsa_tracts, aes(fill = value)) +
    facet_grid(~ measure) +
    geom_sf(lwd = 0.01, color = "grey80") +
    scale_fill_gradient2(midpoint = 0) +
    theme_map() +
    ggsave(filename = paste0("./output/choro/lambda/", 
                             str_split_fixed(metro, "-|,|/", n = 2)[1],
                             "_lambda.pdf"),
           width = 12, height = 8, dpi = 300) 
}

map(metros, choro_ratios)





