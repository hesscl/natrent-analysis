#### Compile extract of tract data with CL/AptsCom counts ---------------------

library(tidyverse)
library(yaml)
library(DBI)
library(sf)

#set wd to project base folder
setwd("R:/Project/natrent-city-sub")

#store credentials at base dir of natrent-city-sub as YAML
cred <- read_yaml("./natrent0.yaml")

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


#### A. Load in Metropolitan ACS Data, Mutate Columns -------------------------

#CBSA level ACS estimates for 2013-2017
acs_cbsa <- read_csv("./input/nhgis0137_csv/nhgis0137_ds239_20185_2018_cbsa.csv") %>%
  select_if(not_all_na) %>%
  mutate(met_name = NAME_E,
         met_id = as.character(CBSAA),
         met_tot_pop = AJWME001,
         met_tot_hu = AJ1SE001,
         met_tot_vac = AJ1TE003,
         met_tot_vac_hu_for_rent = AJ14E002,
         met_tot_wht = AJWVE003,
         met_tot_blk = AJWVE004,
         met_tot_oth = AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009,
         met_tot_lat = AJWVE012,
         met_shr_wht = AJWVE003/AJWVE001,
         met_shr_blk = AJWVE004/AJWVE001,
         met_shr_oth = (AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009)/AJWVE001,
         met_shr_lat = AJWVE012/AJWVE001,
         met_shr_diff_metro_last_yr = ifelse(AJWXE001 == 0, 0, AJWXE007/AJWXE001),
         met_shr_col_grad = (AJYPE022+AJYPE023+AJYPE024+AJYPE025)/AJYPE001,
         met_shr_deep_pov = ifelse(AJY4E001 == 0, 0, AJY4E002/AJY4E001),
         met_shr_pov = ifelse(AJY4E001 == 0, 0, (AJY4E002+AJY4E003)/AJY4E001),
         met_shr_2xfpl = ifelse(AJY4E001 == 0, 0, AJY4E008/AJY4E001),
         met_med_hh_inc = AJZAE001,
         met_shr_hh_pubasst = AJZUE002/AJZUE001,
         met_per_cap_inc = AJ0EE001,
         met_lfp_rate = AJ1CE002/AJ1CE001,
         met_emp_rate = AJ1CE004/AJ1CE001,
         met_shr_comp_math = ((AJ1FE007+AJ1FE044)/AJ1FE001),
         met_shr_vac_hu = AJ1TE003/AJ1TE001,
         met_shr_vac_hu_for_rent = AJ14E002/AJ14E001,
         met_shr_rent_occ = AJ1UE003/AJ1UE001,
         met_shr_sfh = AJ2JE002/AJ2JE001,
         met_shr_20plus = (AJ2JE008+AJ2JE009)/AJ2JE001,
         met_shr_blt_post_2000 = AJ2ME004/AJ2ME001,
         met_med_gross_rent = AJ3EE001,
         met_med_cont_rent = AJ28E001,
         met_med_gross_rent_as_shr_inc = AJ3LE001,
         met_med_own_hu_val = AJ3QE001) %>% 
  rename_all(.funs = tolower) %>%
  select(met_id, cbsa, cbsaa, met_name, starts_with("met")) 


#### B. Load in Neighborhood ACS Data, Mutate Columns --------------------------

#Tract level ACS estimates for 2013-2017
acs_tract <- read_csv("./input/nhgis0137_csv/nhgis0137_ds239_20185_2018_tract.csv") %>%
  select_if(not_all_na) %>%
  mutate(trt_name = NAME_E,
         trt_id = as.character(GISJOIN),
         trt_tot_pop = AJWME001,
         trt_tot_hu = AJ1SE001,
         trt_tot_vac = AJ1TE003,
         trt_tot_vac_hu_for_rent = AJ14E002,
         trt_tot_wht = AJWVE003,
         trt_tot_blk = AJWVE004,
         trt_tot_oth = AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009,
         trt_tot_lat = AJWVE012,
         trt_shr_wht = AJWVE003/AJWVE001,
         trt_shr_blk = AJWVE004/AJWVE001,
         trt_shr_oth = (AJWVE005+AJWVE006+AJWVE007+AJWVE008+AJWVE009)/AJWVE001,
         trt_shr_lat = AJWVE012/AJWVE001,
         trt_shr_diff_metro_last_yr = ifelse(AJWXE001 == 0, 0, AJWXE007/AJWXE001),
         trt_shr_col_grad = (AJYPE022+AJYPE023+AJYPE024+AJYPE025)/AJYPE001,
         trt_shr_deep_pov = ifelse(AJY4E001 == 0, 0, AJY4E002/AJY4E001),
         trt_shr_pov = ifelse(AJY4E001 == 0, 0, (AJY4E002+AJY4E003)/AJY4E001),
         trt_shr_2xfpl = ifelse(AJY4E001 == 0, 0, AJY4E008/AJY4E001),
         trt_med_hh_inc = AJZAE001,
         trt_shr_hh_pubasst = AJZUE002/AJZUE001,
         trt_per_cap_inc = AJ0EE001,
         trt_lfp_rate = AJ1CE002/AJ1CE001,
         trt_emp_rate = AJ1CE004/AJ1CE001,
         trt_shr_comp_math = ((AJ1FE007+AJ1FE044)/AJ1FE001),
         trt_shr_vac_hu = AJ1TE003/AJ1TE001,
         trt_shr_vac_hu_for_rent = AJ14E002/AJ14E001,
         trt_shr_rent_occ = AJ1UE003/AJ1UE001,
         trt_shr_sfh = AJ2JE002/AJ2JE001,
         trt_shr_20plus = (AJ2JE008+AJ2JE009)/AJ2JE001,
         trt_shr_blt_post_2000 = AJ2ME004/AJ2ME001,
         trt_med_gross_rent = AJ3EE001,
         trt_med_cont_rent = AJ28E001,
         trt_med_gross_rent_as_shr_inc = AJ3LE001,
         trt_med_own_hu_val = AJ3QE001) %>% 
  rename_all(.funs = tolower) %>%
  select(trt_id, state, statea, county, countya, tracta, starts_with("trt"))


#### C. Query Neighborhood Estimates of Listing Activity ----------------------

#query for tract aggregates of distinct Craigslist listings
cl_query <- "SELECT f.trt_id, f.met_id, COUNT(*) AS cl_listing_count, QUANTILE(f.clean_rent, .5) AS cl_median,
                    QUANTILE(CASE WHEN f.clean_beds = 0 THEN f.clean_rent END, .50) AS cl_median_0B,
                    QUANTILE(CASE WHEN f.clean_beds = 1 THEN f.clean_rent END, .50) AS cl_median_1B,
                    QUANTILE(CASE WHEN f.clean_beds = 2 THEN f.clean_rent END, .50) AS cl_median_2B,
                    QUANTILE(CASE WHEN f.clean_beds = 3 THEN f.clean_rent END, .50) AS cl_median_3B,
                    QUANTILE(CASE WHEN f.clean_beds >= 4 THEN f.clean_rent END, .50) AS cl_median_4PlusB
             FROM (
                   SELECT DISTINCT e.trt_id, e.met_id, e.clean_beds, e.clean_sqft, e.clean_rent, e.lng, e.lat
                   FROM (
                         SELECT c.trt_id, c.met_id, d.listing_date, d.clean_beds, d.clean_sqft, d.clean_rent,
                                ROUND(CAST(ST_X(ST_TRANSFORM(d.geometry, 4326)) as numeric), 3) as lng, 
                                ROUND(CAST(ST_Y(ST_TRANSFORM(d.geometry, 4326)) as numeric), 3) as lat
                         FROM (
                               SELECT a.gisjoin AS trt_id, a.geometry, b.cbsafp AS met_id
                               FROM tract17 a
                               JOIN county17 b ON a.statefp = b.statefp AND a.countyfp = b.countyfp
                               WHERE b.cbsafp IS NOT NULL
                         ) c
                         LEFT JOIN clean d ON ST_Contains(c.geometry, d.geometry) 
                         WHERE d.listing_date BETWEEN '2019-01-01' AND ?end AND
                               d.match_type NOT IN ('No Address Found')
                         ORDER BY d.listing_date DESC
                        ) e
                  ) f
            GROUP BY f.trt_id, f.met_id
            ORDER BY f.met_id"

#query for tract aggregates of distinct Apts.com listings
apts_query <- "SELECT f.trt_id, f.met_id, COUNT(*) AS apts_listing_count, QUANTILE(clean_rent, .50) AS apts_median,
                    QUANTILE(CASE WHEN f.clean_beds = 0 THEN f.clean_rent END, .50) AS apts_median_0B,
                    QUANTILE(CASE WHEN f.clean_beds = 1 THEN f.clean_rent END, .50) AS apts_median_1B,
                    QUANTILE(CASE WHEN f.clean_beds = 2 THEN f.clean_rent END, .50) AS apts_median_2B,
                    QUANTILE(CASE WHEN f.clean_beds = 3 THEN f.clean_rent END, .50) AS apts_median_3B,
                    QUANTILE(CASE WHEN f.clean_beds >= 4 THEN f.clean_rent END, .50) AS apts_median_4PlusB
               FROM (
                     SELECT DISTINCT e.trt_id, e.met_id, e.clean_beds, e.clean_sqft, e.clean_rent, e.lng, e.lat
                     FROM (
                           SELECT c.trt_id, c.met_id, d.scraped_time, d.clean_beds, d.clean_sqft, d.clean_rent,
                                  ROUND(CAST(ST_X(ST_TRANSFORM(d.geometry, 4326)) as numeric), 3) as lng, 
                                  ROUND(CAST(ST_Y(ST_TRANSFORM(d.geometry, 4326)) as numeric), 3) as lat
                           FROM (
                                 SELECT a.gisjoin AS trt_id, a.geometry, b.cbsafp AS met_id
                                 FROM tract17 a
                                 JOIN county17 b ON a.statefp = b.statefp AND a.countyfp = b.countyfp
                                 WHERE b.cbsafp IS NOT NULL
                           ) c
                           LEFT JOIN apts_clean d ON ST_Contains(c.geometry, d.geometry) 
                           WHERE d.scraped_time BETWEEN '2019-01-01' AND ?end
                           ORDER BY d.scraped_time DESC
                          ) e
                    ) f
              GROUP BY f.trt_id, f.met_id
              ORDER BY f.met_id"

#query for all the tracts in CBSAs even where the cl counts are 0
full_tracts_query <- "SELECT c.trt_id, c.met_id, ST_TRANSFORM(c.geometry, 4326)
                      FROM (
                            SELECT a.gisjoin AS trt_id, a.geometry, b.cbsafp AS met_id
                            FROM tract17 a
                            JOIN county17 b ON a.statefp = b.statefp AND a.countyfp = b.countyfp
                            WHERE b.cbsafp IS NOT NULL
                      ) c"

#set temporal cutoff and work this into queries
cutoff_date <- "2019-12-31"
cl_query <- sqlInterpolate(natrent, cl_query, end = cutoff_date)
apts_query <- sqlInterpolate(natrent, apts_query, end = cutoff_date)

#submit the queries
cl_counts <- dbGetQuery(natrent, cl_query)
apts_counts <- dbGetQuery(natrent, apts_query)

#join up the results
tract_counts <- full_join(cl_counts, apts_counts)

#we want to get the tracts from which we have no listings
full_tracts <- st_read(natrent, query = full_tracts_query)

#then we want to filter to only the met areas where we're collecting listings
tract <- full_tracts %>% 
  filter(met_id %in% unique(tract_counts$met_id)) %>% 
  #join it to the ones where we have counts
  left_join(tract_counts) %>% 
  #then fill the NAs with zeros because we have no listings there
  mutate(cl_listing_count = if_else(is.na(cl_listing_count), 0, cl_listing_count),
         apts_listing_count = if_else(is.na(apts_listing_count), 0, apts_listing_count))


#### D. Join Tables ------------------------------------------------------------

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

#vector for largest 100 by pop
#top100 <- tract %>%
#  st_drop_geometry() %>%
#  distinct(met_id, met_tot_pop) %>%
#  top_n(100, met_tot_pop) %>%
#  pull(met_id)

#### E. Save to Storage -------------------------------------------------------

st_write(tract, "./output/extract/tract_listing_count_thru_2019.geojson", delete_dsn = T)

flat_file <- st_drop_geometry(tract)

write_csv(flat_file, "./output/extract/tract_listing_count_thru_2019.csv")


