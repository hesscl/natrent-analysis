
#dependencies
library(tidyverse)
library(sf)
library(haven)
library(biscale)
library(cowplot)

#working directory = repo base dir
setwd("R:/Project/natrent-city-sub")

#load in tract extract 
tract <- read_sf("./output/extract/tract_listing_count_thru_aug_2019.geojson")


#### C. Map spatial distribution of lambda  -----------------------------------

#first, reproject the tract sf to better output CRS (web mercator)
tract <- st_transform(tract, crs = 102008)

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
  
  ggplot(cbsa_tracts, aes(fill = log(value))) +
    facet_grid(~ measure) +
    geom_sf(color = NA) +
    scale_fill_gradientn(colours = c(scales::muted("red"), "white", scales::muted("blue")), 
                         labels = c("Under", "", "Parity", "", "Over"),
                         breaks = c(5, 2, 0, 2, 5),
                         #values = scales::rescale(c(0, 1, 2)),
                         guide = "colorbar") +
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
    st_transform(crs = 3857) %>%
    mutate(under_cl = 1 - ifelse(cl_lambda > 1, 1, cl_lambda),
           under_apts = 1 - ifelse(apts_lambda > 1, 1, apts_lambda))
  
  base_map <- cbsa_tracts %>%
    select(geometry)
  
  city_sub <- cbsa_tracts %>%
    mutate(city_sub = case_when(prin_city == "Y" ~ "City", 
                                prin_city != "Y" ~ "Suburb")) %>%
    select(city_sub, geometry) %>%
    group_by(city_sub) %>%
    summarize() %>%
    filter(city_sub == "City")
  
  cbsa_tracts <- cbsa_tracts %>%
    filter(trt_tot_rent_hu > 0)
  
  #add the biclass col using fn from biscale lib
  under_tracts <- bi_class(cbsa_tracts, 
                          x = under_apts, y = under_cl,
                          style = "equal", dim = 3)

  #make the legends
  under_legend <- bi_legend(pal = "GrPink",
                      dim = 3,
                      ylab = "Craigslist",
                      xlab = "Apts.com",
                      size = 10) 
  
  #make the under map
  under_choro <- ggplot() +
    geom_sf(data = base_map, fill = "lightgrey", color = NA, show.legend = F) +
    geom_sf(data = under_tracts, aes(fill = bi_class), 
            color = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = city_sub, fill = NA, color = "black", lwd = 1) + 
    bi_scale_fill(pal = "GrPink", dim = 3) +
    labs(title = paste("Underrepresented Neighborhoods in", metro)) +
    bi_theme() +
    theme(plot.title = element_text(size = 14))
  
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

#choro_bivar(metros[85])

#produce the chroropleth for each metropolitan area by mapping
#all unique metro codes to choro_bivar
map(metros, choro_bivar)


