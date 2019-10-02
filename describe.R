
#dependencies
library(tidyverse)
library(sf)

#load in tract extract geojson
tract <- st_read("./output/extract/tract_listing_count_thru_sept.geojson")

