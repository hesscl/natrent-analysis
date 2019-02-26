library(tidyverse)
library(yaml)
library(DBI)

#make sure wd is base of natrent-viewer repo
setwd("H:/natrent-viewer")

#store credentials at base dir of UDrive (H:/) as YAML
cred <- read_yaml("H:/natrent0.yaml")

#create a database connection
natrent <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = "natrent",
  host = "natrent0.csde.washington.edu",
  user = names(cred),
  password = cred[[names(cred)]],
  bigint = "character"
)

#function to use for dropping all NA columns in NHGIS extracts
not_all_na <- function(x) any(!is.na(x))

#### A. Load in Metropolitan Data, Mutate Columns ------------------------------

#CBSA level ACS estimates for 2013-2017
acs_cbsa <- read_csv("H:/nhgis0100_csv/nhgis0100_ds233_20175_2017_cbsa.csv") %>%
  select_if(not_all_na) %>%
  mutate(name = NAME_E,
         met_tot_pop = AHZAE001,
         met_tot_hu = AH35E001,
         met_shr_wht = AHZAE003/AHZAE001,
         met_shr_blk = AHZAE004/AHZAE001,
         met_shr_asn = AHZAE006/AHZAE001,
         met_shr_oth = (AHZAE005+AHZAE007+AHZAE008+AHZAE009+AHZAE010+AHZAE011)/AHZAE001,
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
         met_shr_comp_math = (AH3SE007+AH3SE044/AH3SE001),
         met_shr_vac_hu = AH36E003/AH36E001,
         met_shr_vac_hu_for_rent = AH4HE002/AH4HE001,
         met_shr_rent_occ = AH37E003/AH37E001,
         met_med_num_rooms = AH4RE001,
         met_shr_sfh = AH4WE002/AH4WE001,
         met_shr_20plus = AH4WE008/AH4WE001,
         met_shr_blt_post_2000 = AH4ZE004/AH4ZE001,
         met_med_gross_rent = AH5RE001,
         met_med_cont_rent = AH5LE001,
         met_med_gross_rent_as_shr_inc = AH5YE001,
         met_med_own_hu_val = AH53E001) %>% 
  rename_all(.funs = tolower) %>%
  select(gisjoin, year, cbsa, cbsaa, name, starts_with("met"))


#### B. Load in Neighborhood Data, Mutate Columns

#Tract level ACS estimates for 2013-2017
acs_tract <- read_csv("H:/nhgis0100_csv/nhgis0100_ds233_20175_2017_tract.csv") %>%
  select_if(not_all_na) %>%
  mutate(trt_tot_pop = AHZAE001,
         trt_tot_hu = AH35E001,
         trt_shr_wht = AHZAE003/AHZAE001,
         trt_shr_blk = AHZAE004/AHZAE001,
         trt_shr_asn = AHZAE006/AHZAE001,
         trt_shr_oth = (AHZAE005+AHZAE007+AHZAE008+AHZAE009+AHZAE010+AHZAE011)/AHZAE001,
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
         trt_shr_comp_math = (AH3SE007+AH3SE044/AH3SE001),
         trt_shr_vac_hu = AH36E003/AH36E001,
         trt_shr_vac_hu_for_rent = AH4HE002/AH4HE001,
         trt_shr_rent_occ = AH37E003/AH37E001,
         trt_med_num_rooms = AH4RE001,
         trt_shr_sfh = AH4WE002/AH4WE001,
         trt_shr_20plus = AH4WE008/AH4WE001,
         trt_shr_blt_post_2000 = AH4ZE004/AH4ZE001,
         trt_med_gross_rent = AH5RE001,
         trt_med_cont_rent = AH5LE001,
         trt_med_gross_rent_as_shr_inc = AH5YE001,
         trt_med_own_hu_val = AH53E001) %>% 
  rename_all(.funs = tolower) %>%
  select(gisjoin, year, state, statea, county, countya, tracta, starts_with("trt"))
