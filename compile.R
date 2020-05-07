#### Compile extract of tract data with CL/AptsCom counts ---------------------

library(tidyverse)
library(yaml)
library(DBI)
library(sf)
library(doParallel)

#set wd to project base folder
setwd("R:/Project/natrent-city-sub")

#CBD coordinates
cbd <- read_csv("./input/CBD_geocodes.csv")

#function to use for dropping all NA columns in NHGIS extracts
not_all_na <- function(x) any(!is.na(x))


#### Collect data from natrent database ---------------------------------------

#assemble the Craigslist SQL queries into a list, name elements based on data definition
cl_counts <- list("Beds X Sqft X Loc" = read_file("./sql/cl_beds_sqft_loc_tract.sql"), 
                  "Beds X Sqft X Rent X Loc" = read_file("./sql/cl_beds_sqft_rent_loc_tract.sql"), 
                  "Post ID" = read_file("./sql/cl_post_id_tract.sql"), 
                  "No Dedupe" = read_file("./sql/cl_no_dedupe_tract.sql"), 
                  "No Filter, No Dedupe" = read_file("./sql/cl_no_filter_no_dedupe_tract.sql"))

#assemble the Apartments.com SQL queries into a list, name elements based on data definition
apts_counts <- list("Beds X Sqft X Loc" = read_file("./sql/apts_beds_sqft_loc_tract.sql"), 
                    "Beds X Sqft X Rent X Loc" = read_file("./sql/apts_beds_sqft_rent_loc_tract.sql"), 
                    "No Dedupe" = read_file("./sql/apts_no_dedupe_tract.sql"), 
                    "No Filter, No Dedupe" = read_file("./sql/apts_no_filter_no_dedupe_tract.sql"))

#query for all the tracts in CBSAs even where the cl counts are 0
tract <- read_file("./sql/full_tract_query.sql")

#query for all metro CBSAs 
cbsa <- "SELECT * FROM cbsa17 WHERE memi = '1'"

#function to submit the queries
natrent_query <- function(query, cred = yaml::read_yaml("./natrent0.yaml"), cutoff_date = NULL){
  
  #create a database connection
  conn <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "natrent",
    host = "natrent0.csde.washington.edu",
    user = names(cred),
    password = cred[[names(cred)]],
    bigint = "numeric"
  )

  #interpolate cutoff value
  if(!is.null(cutoff_date)) {
    query <- DBI::sqlInterpolate(conn, query, end = cutoff_date)
  }
  
  #submit statement to database
  sf::st_read(conn, query = query)
  
  #close connection
  DBI::dbDisconnect(conn)
}

#register parallel backend to use 2 cores (half of natrent0's CPUs)
cl <- makeCluster(2)
registerDoParallel(cl)

#submit each query for each platform running two queries in parallel
cl_counts <- foreach(i = names(cl_counts)) %dopar% 
  natrent_query(cl_counts[[i]], cutoff_date = "2019-08-31")

apts_counts <- foreach(i = names(apts_counts)) %dopar% 
  natrent_query(apts_counts[[i]], cutoff_date = "2019-08-31")

#submit queries for boundary shapefiles
tract <- natrent_query(tract)
cbsa <- natrent_query(cbsa)


#### Prepare distance measure for each tract ----------------------------------

#first, make the cbd table into a sf object
cbd <- cbd %>%
  st_as_sf(coords = c("CBDlon", "CBDlat"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(crs = st_crs(cbsa))

#point in polygon for cbd coords with most recent metro shapefile
cbd <- st_join(cbd, cbsa %>% select(met_id = cbsafp))

#adjudicate the cases where small cbsas merged into larger ones by 2017 ACS
cbd <- cbd %>%
  group_by(met_id) %>%
  filter(n() == 1 | 
           PrincipleCity %in% c("Deltona city", "Grand Rapids city", "Greenville city", 
                                "Gulfport city",  "Indianapolis city (balance)", "New York city")) %>%
  st_drop_geometry()

#join by met_id to tracts
tract <- tract %>%
  left_join(cbd %>% select(met_id, CBDlon, CBDlat)) %>%
  mutate(intptlon = type.convert(intptlon),
         intptlat = type.convert(intptlat),
         dist_to_cbd = sqrt((intptlon-CBDlon)^2+(intptlat-CBDlat)^2))


#### Load in Metropolitan ACS Data, Mutate Columns ----------------------------

#CBSA level ACS estimates for 2013-2017
acs_cbsa <- read_csv("./input/nhgis0148_csv/nhgis0148_ds239_20185_2018_cbsa.csv") %>%
  mutate(met_name = NAME_E,
         
         #Boeing 2020 EPA replication/extension variables
         met_id = as.character(CBSAA),
         met_shr_age_20_34 = (AJWBE008+AJWBE009+AJWBE010+AJWBE011+AJWBE012+
                                AJWBE032+AJWBE033+AJWBE034+AJWBE035+AJWBE036)/AJWBE001,
         met_shr_age_65plus = (AJWBE020+AJWBE021+AJWBE022+AJWBE023+AJWBE024+AJWBE025+
                                 AJWBE044+AJWBE045+AJWBE046+AJWBE047+AJWBE048+AJWBE049)/AJWBE001,
         met_shr_blt_pre_1940 = (AJ2ME010+AJ2ME011)/AJ2ME001,
         met_shr_blk = AJWVE004/AJWVE001,
         met_shr_hcb = (AJ3KE007+AJ3KE008+AJ3KE009+AJ3KE010)/AJ3KE001,
         met_avg_time_to_work = AJXAE001/(AJXCE001-AJXCE021),
         met_shr_col_grad = (AJYPE022+AJYPE023+AJYPE024+AJYPE025)/AJYPE001,
         met_tot_pop = AJWME001, #to use for density
         met_shr_eng_only = AJY2E002/AJY2E001,
         met_shr_for_born = AJ4XE005/AJ4XE001,
         met_avg_hh_size = AJ19E003,
         met_shr_lat = AJWVE012/AJWVE001,
         met_med_own_hu_val = AJ3QE001,
         met_med_hh_inc = AJZAE001,
         met_shr_male = AJWBE002/AJWBE001,
         met_shr_nonrel = (AJXHE018+AJXHE032)/AJXHE001,
         met_shr_pov = ifelse(AJY4E001 == 0, 0, (AJY4E002+AJY4E003)/AJY4E001),
         met_med_gross_rent = AJ3EE001,
         met_med_n_rooms = AJ2EE001,
         met_shr_same_home = AJWXE002/AJWXE001,
         met_shr_sfh = AJ2JE002/AJ2JE001, 
         met_shr_col_stud = (AJYEE017+AJYEE018)/AJYEE001,
         met_tot_rent_hu = AJ1UE003+AJ14E002+AJ14E003, #renter-occ, vacant for rent + rented but not occ
         met_vac_rate = AJ14E002/met_tot_rent_hu,
         met_shr_wht = AJWVE003/AJWVE001,
         
         #other columns
         met_tot_vac_hu = AJ1TE003,
         met_tot_vac_hu_for_rent = AJ14E002,
         met_tot_rent_occ = AJ1UE003,
         met_tot_wht = AJWVE003,
         met_tot_blk = AJWVE004,
         met_tot_oth = AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009,
         met_tot_lat = AJWVE012,
         met_shr_oth = (AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009)/AJWVE001,
         met_shr_diff_metro_last_yr = ifelse(AJWXE001 == 0, 0, AJWXE007/AJWXE001),
         met_shr_deep_pov = ifelse(AJY4E001 == 0, 0, AJY4E002/AJY4E001),
         met_shr_2xfpl = ifelse(AJY4E001 == 0, 0, AJY4E008/AJY4E001),
         met_shr_hh_pubasst = AJZUE002/AJZUE001,
         met_per_cap_inc = AJ0EE001,
         met_lfp_rate = AJ1CE002/AJ1CE001,
         met_emp_rate = AJ1CE004/AJ1CE001,
         met_shr_comp_math = ((AJ1FE007+AJ1FE044)/AJ1FE001),
         met_shr_vac_hu = AJ1TE003/AJ1TE001,
         met_shr_rent_occ = AJ1UE003/AJ1UE001,
         met_shr_20plus = (AJ2JE008+AJ2JE009)/AJ2JE001,
         met_shr_blt_post_2000 = (AJ2ME002+AJ2ME003+AJ2ME004)/AJ2ME001,
         met_med_cont_rent = AJ28E001,
         met_med_gross_rent_as_shr_inc = AJ3LE001) %>% 
  rename_all(.funs = tolower) %>%
  select(met_id, cbsa, cbsaa, met_name, starts_with("met")) 


#### Load in Neighborhood ACS Data, Mutate Columns ----------------------------

#Tract level ACS estimates for 2013-2017
acs_tract <- read_csv("./input/nhgis0148_csv/nhgis0148_ds239_20185_2018_tract.csv") %>%
  mutate(trt_name = NAME_E,
         
         #Boeing 2020 EPA replication/extension variables
         trt_id = as.character(GISJOIN),
         trt_shr_age_20_34 = (AJWBE008+AJWBE009+AJWBE010+AJWBE011+AJWBE012+
                                AJWBE032+AJWBE033+AJWBE034+AJWBE035+AJWBE036)/AJWBE001,
         trt_shr_age_65plus = (AJWBE020+AJWBE021+AJWBE022+AJWBE023+AJWBE024+AJWBE025+
                                 AJWBE044+AJWBE045+AJWBE046+AJWBE047+AJWBE048+AJWBE049)/AJWBE001,
         trt_shr_blt_pre_1940 = (AJ2ME010+AJ2ME011)/AJ2ME001,
         trt_shr_blk = AJWVE004/AJWVE001,
         trt_shr_hcb = (AJ3KE007+AJ3KE008+AJ3KE009+AJ3KE010)/AJ3KE001,
         trt_agg_time_to_work = AJXAE001,
         trt_tot_workers = AJXCE001,
         trt_tot_work_from_home = AJXCE021,
         trt_tot_workers_commuting = if_else(trt_tot_workers < trt_tot_work_from_home, 0, 
                                             trt_tot_workers - trt_tot_work_from_home),
         trt_shr_col_grad = (AJYPE022+AJYPE023+AJYPE024+AJYPE025)/AJYPE001,
         trt_tot_pop = AJWME001, #to use for density
         trt_shr_eng_only = AJY2E002/AJY2E001,
         trt_shr_for_born = AJ4XE005/AJ4XE001,
         trt_avg_hh_size = AJ19E003,
         trt_shr_lat = AJWVE012/AJWVE001,
         trt_med_own_hu_val = AJ3QE001,
         trt_med_hh_inc = AJZAE001,
         trt_shr_male = AJWBE002/AJWBE001,
         trt_shr_nonrel = (AJXHE018+AJXHE032)/AJXHE001,
         trt_shr_pov = ifelse(AJY4E001 == 0, 0, (AJY4E002+AJY4E003)/AJY4E001),
         trt_med_gross_rent = AJ3EE001,
         trt_med_n_rooms = AJ2EE001,
         trt_shr_same_home = AJWXE002/AJWXE001,
         trt_shr_sfh = AJ2JE002/AJ2JE001,
         trt_shr_col_stud = (AJYEE017+AJYEE018)/AJYEE001,
         trt_tot_rent_hu = AJ1UE003+AJ14E002+AJ14E003, #renter-occ, vacant for rent + rented but not occ
         trt_vac_rate = AJ14E002/trt_tot_rent_hu,
         trt_shr_wht = AJWVE003/AJWVE001,
         
         #other columns
         trt_tot_vac_hu = AJ1TE003,
         trt_tot_vac_hu_for_rent = AJ14E002,
         trt_tot_rent_occ = AJ1UE003,
         trt_tot_wht = AJWVE003,
         trt_tot_blk = AJWVE004,
         trt_tot_oth = AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009,
         trt_tot_lat = AJWVE012,
         trt_shr_oth = (AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009)/AJWVE001,
         trt_shr_diff_metro_last_yr = ifelse(AJWXE001 == 0, 0, AJWXE007/AJWXE001),
         trt_shr_deep_pov = ifelse(AJY4E001 == 0, 0, AJY4E002/AJY4E001),
         trt_shr_2xfpl = ifelse(AJY4E001 == 0, 0, AJY4E008/AJY4E001),
         trt_shr_hh_pubasst = AJZUE002/AJZUE001,
         trt_per_cap_inc = AJ0EE001,
         trt_lfp_rate = AJ1CE002/AJ1CE001,
         trt_emp_rate = AJ1CE004/AJ1CE001,
         trt_shr_comp_math = ((AJ1FE007+AJ1FE044)/AJ1FE001),
         trt_shr_vac_hu = AJ1TE003/AJ1TE001,
         trt_shr_rent_occ = AJ1UE003/AJ1UE001,
         trt_shr_20plus = (AJ2JE008+AJ2JE009)/AJ2JE001,
         trt_shr_blt_post_2000 = (AJ2ME002+AJ2ME003+AJ2ME004)/AJ2ME001,
         trt_med_cont_rent = AJ28E001,
         trt_med_gross_rent_as_shr_inc = AJ3LE001,
         trt_med_yr_hu_blt = AJ2NE001,
         trt_shr_rent_occ_blt_post_2000 = (AJ2OE014+AJ2OE015+AJ2OE016)/AJ2OE001,
         trt_med_yr_rent_hu_blt = AJ2PE003) %>% 
  rename_all(.funs = tolower) %>%
  select(trt_id, state, statea, county, countya, tracta, starts_with("trt"))


#### Join up data -----------------------------------------------------

#then we want to filter to only the met areas where we're collecting listings
tract <- tract %>% 
  filter(met_id %in% unique(tract_counts$met_id)) %>% 
  #join it to the ones where we have counts
  left_join(tract_counts) %>% 
  #then fill the NAs with zeros because we have no listings there
  mutate(cl_listing_count = if_else(is.na(cl_listing_count), 0, cl_listing_count),
         apts_listing_count = if_else(is.na(apts_listing_count), 0, apts_listing_count))

tract <- inner_join(tract, acs_tract)

tract <- left_join(tract, acs_cbsa)

tract <- tract %>%
  group_by(met_id) %>%
  mutate(met_cl_listing_count = sum(cl_listing_count),
         met_apts_listing_count = sum(apts_listing_count)) %>%
  ungroup() %>%
  filter(state != "Puerto Rico") %>%
  mutate(any_cl_listings = cl_listing_count > 0,
         any_apts_listings = apts_listing_count > 0)

#make variables for boeing replication
tract <- tract %>% 
  group_by(met_id) %>% 
  mutate(met_tot_cl_listings = sum(cl_listing_count),
         met_tot_apts_listings = sum(apts_listing_count),
         phi_tract_cl = (met_tot_cl_listings)*((trt_tot_vac_hu_for_rent)/sum(trt_tot_vac_hu_for_rent)),
         phi_tract_apts = (met_tot_apts_listings)*((trt_tot_vac_hu_for_rent)/sum(trt_tot_vac_hu_for_rent)),
         count_tracts = n()) %>% 
  ungroup() %>% 
  mutate(cl_lambda = (cl_listing_count+1)/(phi_tract_cl+1),
         apts_lambda = (apts_listing_count+1)/(phi_tract_apts+1)) 


#### Filter to top 100, add boeing sample filter ------------------------------

top_100 <- tract %>%
  st_drop_geometry() %>%
  filter(met_tot_cl_listings > 0) %>%
  distinct(met_id, met_tot_pop) %>%
  top_n(100, met_tot_pop) %>%
  arrange(desc(met_tot_pop)) %>%
  pull(met_id)

tract <- tract %>%
  filter(met_id %in% top_100) %>%
  mutate(top_50 = met_id %in% top_100[1:50])


#### Pivot data longer by platform --------------------------------------------

cl_mdata <- tract %>%
  select(everything(), -apts_lambda) %>%
  mutate(platform = "Craigslist") %>%
  rename(lambda = cl_lambda)

apts_mdata <- tract %>%
  select(everything(), -cl_lambda) %>%
  mutate(platform = "Apartments.com") %>%
  rename(lambda = apts_lambda)

tract_mdata <- bind_rows(cl_mdata, apts_mdata) %>%
  group_by(met_id, platform) %>%
  mutate(dis_blk_wht = (.5) * sum(abs(trt_tot_blk/sum(trt_tot_blk) - 
                                        trt_tot_wht/sum(trt_tot_wht))),
         dis_lat_wht = (.5) * sum(abs(trt_tot_lat/sum(trt_tot_lat) - 
                                        trt_tot_wht/sum(trt_tot_wht)))) %>%
  ungroup()


#### Filter to tracts with rental housing -------------------------------------

tract_mdata <- tract_mdata %>%
  filter(trt_tot_rent_hu > 0)

tract <- tract %>%
  mutate(cl_lambda = ifelse(trt_tot_rent_hu == 0, NA, cl_lambda),
         apts_lambda = ifelse(trt_tot_rent_hu == 0, NA, apts_lambda))


#### Save data to storage -----------------------------------------------------

st_write(st_transform(tract, 4326), "./output/extract/tract_listing_count_thru_aug_2019.geojson", 
         delete_dsn = T)

flat_file <- st_drop_geometry(tract)

write_csv(flat_file, "./output/extract/tract_listing_count_thru_aug_2019.csv")

#save data for models
save(tract_mdata, file = "./output/extract/tract_mdata.RData")


