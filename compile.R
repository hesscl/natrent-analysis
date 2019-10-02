#### Compile extract of tract data with CL/AptsCom counts ---------------------

library(tidyverse)
library(yaml)
library(DBI)
library(sf)

#store credentials at base dir of UDrive (H:/) as YAML
cred <- read_yaml("H:/natrent0.yaml")

#create a database connection
natrent <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "natrent",
  host = "natrent0.csde.washington.edu",
  user = names(cred),
  password = cred[[names(cred)]],
  bigint = "numeric"
)

#function to use for dropping all NA columns in NHGIS extracts
not_all_na <- function(x) any(!is.na(x))


#### A. Load in Metropolitan ACS Data, Mutate Columns --------------------------

#CBSA level ACS estimates for 2013-2017
acs_cbsa <- read_csv("H:/nhgis0100_csv/nhgis0100_ds233_20175_2017_cbsa.csv") %>%
  select_if(not_all_na) %>%
  mutate(met_name = NAME_E,
         met_id = as.character(CBSAA),
         met_tot_pop = AHZAE001,
         met_tot_hu = AH35E001,
         met_shr_wht = AHZAE003/AHZAE001,
         met_shr_blk = AHZAE004/AHZAE001,
         met_shr_oth = (AHZAE005+AHZAE006+AHZAE007+AHZAE008+AHZAE009)/AHZAE001,
         met_shr_lat = AHZAE012/AHZAE001,
         met_shr_diff_metro_last_yr = ifelse(AHZCE001 == 0, 0, AHZCE007/AHZCE001),
         met_shr_col_grad = (AH04E022+AH04E023+AH04E024+AH04E025)/AH04E001,
         met_shr_deep_pov = ifelse(AH1JE001 == 0, 0, AH1JE002/AH1JE001),
         met_shr_pov = ifelse(AH1JE001 == 0, 0, (AH1JE002+AH1JE003)/AH1JE001),
         met_shr_2xfpl = ifelse(AH1JE001 == 0, 0, AH1JE008/AH1JE001),
         met_med_hh_inc = AH1PE001,
         met_shr_hh_pubasst = AH19E002/AH19E001,
         met_per_cap_inc = AH2RE001,
         met_lfp_rate = AH3PE002/AH3PE001,
         met_emp_rate = AH3PE004/AH3PE001,
         met_shr_comp_math = ((AH3SE007+AH3SE044)/AH3SE001),
         met_shr_vac_hu = AH36E003/AH36E001,
         met_shr_vac_hu_for_rent = AH4HE002/AH4HE001,
         met_shr_rent_occ = AH37E003/AH37E001,
         met_med_num_rooms = AH4RE001,
         met_shr_sfh = AH4WE002/AH4WE001,
         met_shr_20plus = (AH4WE008+AH4WE009)/AH4WE001,
         met_shr_blt_post_2000 = AH4ZE004/AH4ZE001,
         met_med_gross_rent = AH5RE001,
         met_med_cont_rent = AH5LE001,
         met_med_gross_rent_as_shr_inc = AH5YE001,
         met_med_own_hu_val = AH53E001) %>% 
  rename_all(.funs = tolower) %>%
  select(met_id, cbsa, cbsaa, met_name, starts_with("met")) 


#### B. Load in Neighborhood ACS Data, Mutate Columns --------------------------

#Tract level ACS estimates for 2013-2017
acs_tract <- read_csv("H:/nhgis0100_csv/nhgis0100_ds233_20175_2017_tract.csv") %>%
  select_if(not_all_na) %>%
  mutate(trt_id = GISJOIN,
         trt_tot_pop = AHZAE001,
         trt_tot_hu = AH35E001,
         trt_shr_wht = AHZAE003/AHZAE001,
         trt_shr_blk = AHZAE004/AHZAE001,
         trt_shr_oth = (AHZAE005+AHZAE006+AHZAE007+AHZAE008+AHZAE009)/AHZAE001,
         trt_shr_lat = AHZAE012/AHZAE001,
         trt_shr_diff_trtro_last_yr = ifelse(AHZCE001 == 0, 0, AHZCE007/AHZCE001),
         trt_shr_col_grad = (AH04E022+AH04E023+AH04E024+AH04E025)/AH04E001,
         trt_shr_deep_pov = ifelse(AH1JE001 == 0, 0, AH1JE002/AH1JE001),
         trt_shr_pov = ifelse(AH1JE001 == 0, 0, (AH1JE002+AH1JE003)/AH1JE001),
         trt_shr_2xfpl = ifelse(AH1JE001 == 0, 0, AH1JE008/AH1JE001),
         trt_med_hh_inc = AH1PE001,
         trt_shr_hh_pubasst = AH19E002/AH19E001,
         trt_per_cap_inc = AH2RE001,
         trt_lfp_rate = AH3PE002/AH3PE001,
         trt_emp_rate = AH3PE004/AH3PE001,
         trt_shr_comp_math = ((AH3SE007+AH3SE044)/AH3SE001),
         trt_shr_vac_hu = AH36E003/AH36E001,
         trt_tot_vac_hu_for_rent = AH4HE002,
         trt_shr_vac_hu_for_rent = AH4HE002/AH4HE001,
         trt_shr_rent_occ = AH37E003/AH37E001,
         trt_med_num_rooms = AH4RE001,
         trt_shr_sfh = AH4WE002/AH4WE001,
         trt_shr_20plus = (AH4WE008+AH4WE009)/AH4WE001,
         trt_shr_blt_post_2000 = AH4ZE004/AH4ZE001,
         trt_med_gross_rent = AH5RE001,
         trt_med_cont_rent = AH5LE001,
         trt_med_gross_rent_as_shr_inc = AH5YE001,
         trt_med_own_hu_val = AH53E001) %>% 
  rename_all(.funs = tolower) %>%
  select(trt_id, state, statea, county, countya, tracta, starts_with("trt"))


#### C. Query Neighborhood Estimates of CL Listing Activity --------------------

#query for tract aggregates of Craigslist listing
cl_query <- "SELECT c.trt_id, c.met_id, count(d.*) AS listing_count, c.geometry
             FROM (
                   SELECT a.gisjoin AS trt_id, a.geometry, b.cbsafp AS met_id
                   FROM tract17 a
                   JOIN county17 b ON a.statefp = b.statefp AND a.countyfp = b.countyfp
                   WHERE b.cbsafp IS NOT NULL
             ) c
             LEFT JOIN clean d ON ST_Contains(c.geometry, d.geometry) WHERE d.listing_date BETWEEN '2019-01-01' and ?end
             GROUP BY c.trt_id, c.met_id, c.geometry
             ORDER BY c.met_id"

#query for tract aggregates of Apartments.com listings (no geom)
apts_query <- "SELECT c.trt_id, count(d.*) AS apts_listing_count
               FROM (
                     SELECT a.gisjoin AS trt_id, a.geometry, b.cbsafp AS met_id
                     FROM tract17 a
                     JOIN county17 b ON a.statefp = b.statefp AND a.countyfp = b.countyfp
                     WHERE b.cbsafp IS NOT NULL
               ) c
               LEFT JOIN apts_clean d ON ST_Contains(c.geometry, d.geometry) WHERE d.scraped_time BETWEEN '2019-01-01' and ?end
               GROUP BY c.trt_id, c.met_id, c.geometry
               ORDER BY c.met_id"

#query for all the tracts in CBSAs even where the cl counts are 0
full_tracts_query <- "SELECT c.trt_id, c.met_id, c.geometry
                      FROM (
                            SELECT a.gisjoin AS trt_id, a.geometry, b.cbsafp AS met_id
                            FROM tract17 a
                            JOIN county17 b ON a.statefp = b.statefp AND a.countyfp = b.countyfp
                            WHERE b.cbsafp IS NOT NULL
                      ) c"

#set temporal cutoff and work this into queries
cutoff_date <- "2019-09-30"
cl_query <- sqlInterpolate(natrent, cl_query, end = cutoff_date)
apts_query <- sqlInterpolate(natrent, apts_query, end = cutoff_date)

#submit the queries
cl_counts <- st_read(natrent, query = cl_query)
apts_counts <- dbGetQuery(natrent, apts_query)

#join up the results
tract_counts <- full_join(cl_counts, apts_counts)

#we want to get the tracts from which we have no listings
full_tracts <- st_read(natrent, query = full_tracts_query)
tract_no_geo <- st_drop_geometry(tract_counts)

#then we want to filter to only the met areas where we're collecting listings
tract <- full_tracts %>% 
  filter(met_id %in% unique(tract_no_geo$met_id)) %>% 
  #join it to the ones where we have counts
  left_join(st_drop_geometry(tract_counts)) %>% 
  #then fill the NAs with zeros because we have no listings there
  mutate(listing_count = if_else(is.na(listing_count), 0, listing_count))


#### D. Join Tables ------------------------------------------------------------

tract <- inner_join(tract, acs_tract)

tract <- left_join(tract, acs_cbsa)

tract <- tract %>%
  group_by(met_id) %>%
  mutate(met_listing_count = sum(listing_count)) %>%
  ungroup() %>%
  mutate(any_listings = listing_count > 0)

top100 <- tract %>%
  select(met_id, met_name, met_tot_pop) %>%
  st_drop_geometry() %>%
  distinct() %>%
  top_n(100, met_tot_pop) %>%
  arrange(desc(met_tot_pop))

tract <- tract %>%
  filter(met_id %in% top100$met_id)


#### E. Save to Storage -------------------------------------------------------

st_write(tract, "./output/extract/tract_listing_count_thru_sept.geojson", delete_dsn = T)

flat_file <- st_drop_geometry(tract)

write_csv(flat_file, "./output/extract/tract_listing_count_thru_sept.csv")

