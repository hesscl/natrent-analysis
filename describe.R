
#dependencies
library(tidyverse)
library(sf)
library(haven)

#load in tract extract geojson
tract <- st_read("./output/extract/tract_listing_count_thru_sept.geojson")

#load in tract panel
tract_panel <- read_dta("../dissertation/metro-povtrends/panel_for_stata.dta")

#check coverage, 0.17% of tracts w/o match in tract extract from natrent
#due to small changes in ACS tracts
tract_panel %>%
  filter(year == 2015) %>%
  pull(GISJOIN) %in% tract$trt_id %>% 
  table(., useNA = "always")/nrow(tract_panel %>% filter(year == 2015))


#### A. Join up ---------------------------------------------------------------

#mutate a common name key for join, reduce table to necessary columns
tract_panel <- tract_panel %>%
  mutate(trt_id = GISJOIN) %>%
  distinct(trt_id, clust)

#join data
tract <- left_join(tract, tract_panel) %>%
  st_as_sf() 

#vector for largest 100 by pop
top100 <- tract %>%
  st_drop_geometry() %>%
  distinct(met_id, met_tot_pop) %>%
  top_n(100, met_tot_pop) %>%
  pull(met_id)

#use vector of CBSA codes for largest 100 to filter tract sf
tract <- tract %>%
  filter(met_id %in% top100)


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
         cl_rel_ratio = cl_listing_count/expected_cl_listings,
         apts_rel_ratio = apts_listing_count/expected_apts_listings) 


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

#now save a function to pass to map with the vector of CBSA codes
choro_ratios <- function(cbsa){
  cbsa_tracts <- tract %>%
    mutate(met_id = as.character(met_id)) %>%
    filter(met_id == cbsa) %>%
    select(cl_rel_ratio, apts_rel_ratio)
  
  ggplot(cbsa_tracts, aes(fill = log10(cl_rel_ratio))) +
  geom_sf(lwd = 0.01) +
  scale_fill_gradient2(midpoint = 0) +
  theme_map() +
  ggsave(filename = paste0("./output/choro/", cbsa, "_cl_rel_ratio.pdf"),
         width = 8, height = 8, dpi = 300)  
}


map(unique(tract$met_id), choro_ratios)

#Seattle CBSA lambdas
sea_tracts <- tract %>%
  filter(met_id == "42660") %>%
  select(cl_rel_ratio, apts_rel_ratio) 

ggplot(sea_tracts, aes(fill = log10(cl_rel_ratio))) +
  geom_sf(lwd = 0.01) +
  scale_fill_gradient2(midpoint = 0) +
  theme_map() +
  ggsave(filename = "./output/choro/sea_cl_rel_ratio.pdf",
         width = 8, height = 8, dpi = 300)

ggplot(sea_tracts, aes(fill = log10(apts_rel_ratio))) +
  geom_sf(lwd = 0.01) +
  scale_fill_gradient2(midpoint = 0) +
  theme_map() +
  ggsave(filename = "./output/choro/sea_apts_rel_ratio.pdf",
         width = 8, height = 8, dpi = 300)





