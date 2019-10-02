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
cl_query <- "SELECT c.trt_id, c.met_id, MIN(d.listing_loc) AS listing_loc, count(d.*) AS listing_count, c.geometry
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

#we actually want to get the tracts from which we have no listings
full_tracts <- st_read(natrent, query = full_tracts_query)
tract_no_geo <- st_drop_geometry(tract_counts)

# then we want to filter to only the met areas where we're collecting listings
tract <- full_tracts %>% 
  filter(met_id %in% unique(tract_no_geo$met_id)) %>% 
  # join it to the ones where we have counts
  left_join(st_drop_geometry(tract_counts)) %>% 
  # then fill the NAs with zeros because we have no listings there
  mutate(listing_count = if_else(is.na(listing_count), 0, listing_count))


#### D. Join Tables ------------------------------------------------------------

tract <- inner_join(tract, acs_tract)

tract <- left_join(tract, acs_cbsa)


#### E. Describe Correlations --------------------------------------------------

tract <- tract %>%  mutate(trt_tot_vac_hu_for_rent_1 = if_else(trt_tot_vac_hu_for_rent==0, 1, trt_tot_vac_hu_for_rent))

library(GGally)

tract %>% ggplot(aes(x = listing_count, fill = if_else(listing_count>6000, listing_loc, 'other')))+
  geom_histogram()+
  scale_y_continuous(breaks = c(1,10,100,1000,10000), trans = scales::log1p_trans())+
  ylab("Count in Log Scale")+
  ggtitle("Histogram of Listing counts")



# try looking at relative values
tract <- tract %>% mutate(rel_income = trt_med_hh_inc/met_med_hh_inc, 
                 rel_wht = trt_shr_wht/met_shr_wht, 
                 rel_blk = trt_shr_blk/met_shr_blk, 
                 rel_pov = trt_shr_pov/met_shr_pov)

tract %>% 
  #filter(state == 'New York') %>%
  select(listing_count,
         trt_tot_pop,
         trt_tot_hu,
         trt_shr_vac_hu,
         trt_shr_vac_hu_for_rent,
         trt_shr_wht,
         trt_shr_blk,
         trt_shr_lat,
         trt_shr_col_grad,
         trt_med_hh_inc,
         trt_shr_pov,
         met_tot_pop,
         met_tot_hu,
         met_shr_vac_hu,
         met_shr_vac_hu_for_rent,
         met_shr_wht,
         met_shr_blk,
         met_shr_lat,
         met_shr_col_grad,
         met_med_hh_inc,
         met_shr_pov,
         rel_income,
         rel_wht,
         rel_blk,
         rel_pov
  ) %>%
  as.data.frame() %>% ggcorr()
  
  
#### F. Run Basic Models  
  
# poisson model baseline
lm_0 <- glm(listing_count ~ trt_shr_blk + trt_shr_lat + trt_shr_oth +
              met_tot_pop + met_shr_blk + met_shr_lat + met_shr_oth + offset(log(trt_tot_hu)),
            #offset = tract$trt_tot_vac_hu_for_rent,
            data = tract %>% mutate(trt_tot_hu = if_else(trt_tot_hu<listing_count, listing_count +1 ,trt_tot_hu +1)),
            family = "poisson")

# NB model baseline
MASS::glm.nb(listing_count ~ 1 + offset(trt_tot_hu),
             data = tract %>% mutate(trt_tot_hu = trt_tot_hu+1))

summary(lm_0)

#### G. Try BMA
library(BMA)

vars <- c(#'trt_tot_pop',
          'trt_tot_hu',
          'trt_shr_vac_hu',
          'trt_shr_vac_hu_for_rent',
          'trt_shr_wht',
          'trt_shr_blk',
          'trt_shr_lat',
          'trt_shr_col_grad',
          'trt_med_hh_inc',
          'trt_shr_pov',
          'trt_shr_rent_occ',
          #'trt_shr_sfh',
          'trt_shr_20plus',
          'trt_emp_rate',
          #'trt_lfp_rate',
          #'met_tot_pop',
          #'met_tot_hu',
          #'met_shr_vac_hu',
          'met_shr_vac_hu_for_rent',
          'met_shr_wht',
          'met_shr_blk',
          'met_shr_lat',
          'met_shr_col_grad',
          'met_med_hh_inc',
          'met_shr_pov',
          'met_shr_rent_occ',
          #'met_shr_sfh',
          'met_shr_20plus',
          #'met_emp_rate',
          'met_lfp_rate')
          #'rel_income',
          #'rel_wht',
          #'rel_blk',
          #'rel_pov')

scale_vars <- c('trt_tot_hu',
  #'trt_shr_vac_hu',
  # 'trt_shr_vac_hu_for_rent',
  # 'trt_shr_wht',
  # 'trt_shr_blk',
  # 'trt_shr_lat',
  # 'trt_shr_col_grad',
  'trt_med_hh_inc',
  'trt_med_gross_rent',
  #'trt_shr_pov',
  #'trt_shr_rent_occ',
  #'trt_shr_sfh',
  #'trt_shr_20plus',
  #'trt_emp_rate',
  #'trt_lfp_rate',
  #'met_tot_pop',
  #'met_tot_hu',
  #'met_shr_vac_hu',
  # 'met_shr_vac_hu_for_rent',
  # 'met_shr_wht',
  # 'met_shr_blk',
  # 'met_shr_lat',
  # 'met_shr_col_grad',
  'met_med_hh_inc')
  # 'met_shr_pov',
  # 'met_shr_rent_occ',
  #'met_shr_sfh',
  # 'met_shr_20plus',
  # 'met_emp_rate',
  # 'met_lfp_rate')

test_model <- bic.glm(x = tract_no_geo[vars], y = tract_no_geo$listing_count, offset = log(tract_no_geo$trt_tot_vac_hu_for_rent), glm.family = 'poisson', factor.type = FALSE)

imageplot.bma(test_model)

#### H. Boeing Replication

# make a geometry free version for easy messing about
tract_no_geo <- st_drop_geometry(tract) %>% drop_na(vars)

# make variables for boeing replication
## USE REL_RATIO REP RATIO IS DIFFERENT FROM BOEING
tract_no_geo <- tract_no_geo %>% 
  mutate(met_to_rent = met_shr_vac_hu_for_rent*met_tot_hu, vac_hu_ratio = (trt_tot_vac_hu_for_rent+1)/(met_to_rent+1)) %>% 
  group_by(met_id) %>% mutate(met_tot_listings= sum(listing_count), count_tracts = n()) %>% 
  ungroup() %>% 
  mutate(listing_ratio = (listing_count+1)/met_tot_listings, 
         rep_ratio = listing_ratio/vac_hu_ratio, log_rep_ratio = log(rep_ratio),
         expected_listings = met_tot_listings*vac_hu_ratio,
         rel_ratio = listing_count/expected_listings)

# look at distribution/bivarite plot
tract_no_geo %>% ggplot(aes(trt_shr_wht>.5, rep_ratio))+geom_boxplot()+scale_y_log10()
tract_no_geo %>% ggplot(aes(trt_shr_wht, rep_ratio))+geom_point()

# try baseline models with these measures
glm1 <- glm(paste('log_rep_ratio ~', paste(vars, collapse = '+')), data = tract_no_geo)
nb1 <- MASS::glm.nb(paste('listing_count ~', paste(vars, collapse = '+')), data = tract_centered)

## REPRODUCE BOEING VIS

long_full <- top_no_geo %>% mutate(white_grad = trt_shr_wht*trt_shr_col_grad) %>% gather(key,value, trt_med_hh_inc, trt_med_gross_rent, trt_shr_pov, trt_shr_col_grad, trt_shr_20plus, trt_shr_lat, trt_shr_wht, trt_shr_blk, white_grad)

cutoff = 1

long <- long_full %>% group_by(key, rel_ratio>=cutoff) %>% drop_na(value) %>% mutate(rel_mean = mean(value, na.rm = TRUE), max = max(value), min = min(value)) %>% ungroup() %>% 
  inner_join(tibble(
    key = c("trt_med_hh_inc", "trt_med_gross_rent", "trt_shr_pov", "trt_shr_col_grad", "trt_shr_20plus", "trt_shr_lat", "trt_shr_wht", "trt_shr_blk", "white_grad"),
    full_name = c("Tract Median Household Income", "Tract Median Gross Rent", "Tract Share Poverty", "Tract Share College Grad", "Tract Share Units in Buildings with >20 Units", "Tract Share Latinx", "Tract Share White", "Tract Share Black", "Share White * Share College Grad")
  ), by = "key")

long %>% 
  #filter(key == 'trt_shr_wht') %>%
  ggplot(aes(value, y = ..density.., color = rel_ratio>=cutoff, group = rel_ratio>=cutoff))+
  geom_vline(aes(xintercept = rel_mean, color = rel_ratio>=cutoff), size = 1.5, show.legend = FALSE)+
  geom_freqpoly(size = 2)+
  scale_color_manual(values=c('#4B2E83','#85754d'), name = "Tract Observed Listings >= Expected Listings")+
  theme_minimal()+
  theme(legend.position = 'bottom', axis.title.x = element_blank())+
  facet_wrap(~full_name, scales = 'free', strip.position = 'bottom')+
  ggtitle("Distribution of Tract-Level Co-Variates by Observed vs. Expected Listings")


## Make some simulated data to see why our distributions look different from Boeing's
bump <- c(1,3,5,9,100000)

sim_data <- tibble(
  tract = 1:1000,
  met = rep(1:10, each = 100),
  hu = rnegbin(1000, 39, 1)
) %>% group_by(met) %>% mutate(met_rate = runif(1), met_to_rent = sum(hu)) %>% ungroup() %>% 
  mutate(one_week = (hu*met_rate)+bump[sample.int(5,1)], five_weeks = (hu*met_rate)+bump[sample.int(5,1)]*5) %>% group_by(met) %>% mutate(met_tot_listings_one = sum(one_week), met_tot_listings_five=sum(five_weeks))

sim_data <- sim_data %>% 
  mutate(vac_hu_ratio = (hu+1)/(met_to_rent+1)) %>% 
  mutate(listing_ratio_one = (one_week+1)/met_tot_listings_one,
         expected_listings_one = met_tot_listings_one*vac_hu_ratio,
         rel_ratio_one = one_week/expected_listings_one,
         listing_ratio_five = (five_weeks+1)/met_tot_listings_five,
         expected_listings_five = met_tot_listings_five*vac_hu_ratio,
         rel_ratio_five = five_weeks/expected_listings_five)

sim_data %>% gather(key, value, -tract, -met) %>% ggplot(aes(value))+geom_freqpoly()+facet_wrap(~key, scales = 'free')



#### I. Looking at differences by met area

library(forcats)
library(arm)

# make versions of the datat limited to the top 100 metros and with some scaled variables

top_metros <- tract_no_geo %>% distinct(met_name, .keep_all = TRUE) %>% arrange(desc(met_tot_pop)) %>% head(100) %>% pull(met_name)

tract_centered <- tract_no_geo %>% mutate_at(scale_vars, scale)
top_no_geo <- tract_no_geo %>% filter(met_name %in% top_metros)
top_centered <- tract_centered%>% filter(met_name %in% top_metros)

top_no_geo %>% group_by(met_name) %>% summarize(mean = mean(listing_count/trt_tot_pop), ci = 1.97*sd(listing_count/trt_tot_pop)) %>% 
  ggplot(aes(fct_reorder(met_name, mean), mean, ymin = if_else(mean-ci<0,0,mean-ci), ymax = mean+ci))+
  geom_point()+
  geom_errorbar()+
  ggtitle("Average Listings per person by Listing Location")+
  ylab("listings/tract population")+
  xlab('Listing location')+
  ylim(0,.6)+
  theme(axis.text.x = element_text(angle = 90))

top_no_geo %>% mutate(listing_count = listing_count/trt_tot_pop) %>% 
  ggplot(aes(fct_reorder(met_name, listing_count), listing_count))+
  geom_boxplot()+
  theme(legend.position = 'none')+
  ylim(0,10)+
  ggtitle("Average Listings per Vacant Housing Unit by Listing Location")+
  ylab("listings/vacant housing units")+
  xlab('Listing location')+
  theme(axis.text.x = element_text(angle = 90))


# and model those differences with random slopes and intercepts

library(lme4)

glm_base <- glm(listing_count ~ offset(log(trt_tot_vac_hu_for_rent_1)), family = poisson(), data = top_centered)
lmer_base <- glmer(formula = listing_count ~ offset(log(trt_tot_vac_hu_for_rent_1)) + (1|listing_loc), family = poisson, data = top_centered)

glm1 <- glm(listing_count ~ trt_tot_hu + trt_emp_rate + trt_shr_diff_trtro_last_yr + trt_shr_rent_occ + trt_med_gross_rent + trt_med_hh_inc + trt_shr_20plus + trt_shr_lat + trt_shr_pov + trt_shr_col_grad + trt_shr_wht + trt_shr_blk + offset(log(trt_tot_vac_hu_for_rent_1)), family = poisson(), data = top_centered)

lmer1 <- glmer(listing_count ~ trt_tot_hu + trt_emp_rate + trt_shr_diff_trtro_last_yr + trt_shr_rent_occ + trt_med_gross_rent + trt_med_hh_inc + trt_shr_20plus + trt_shr_lat + trt_shr_pov + trt_shr_col_grad + trt_shr_wht + trt_shr_blk + offset(log(trt_tot_vac_hu_for_rent_1)) + (1 | met_name), family = poisson(), data = top_centered, control = glmerControl(optimizer = c('bobyqa', 'bobyqa')))


lmer2 <- glmer(listing_count ~ trt_tot_hu + trt_emp_rate + trt_shr_diff_trtro_last_yr + trt_shr_rent_occ + trt_med_gross_rent + trt_med_hh_inc + trt_shr_20plus + trt_shr_lat + trt_shr_pov + trt_shr_col_grad + trt_shr_wht + trt_shr_blk + offset(log(trt_tot_vac_hu_for_rent_1)) + (trt_shr_wht + trt_shr_col_grad | met_name), family = poisson(), data = top_centered)

lmer2_50 <- glmer(listing_count ~ trt_tot_hu + trt_emp_rate + trt_shr_diff_trtro_last_yr + trt_shr_rent_occ + trt_med_gross_rent + trt_med_hh_inc + trt_shr_20plus + trt_shr_lat + trt_shr_pov + trt_shr_col_grad + trt_shr_wht + trt_shr_blk + offset(log(trt_tot_vac_hu_for_rent_1)) + (trt_shr_wht + trt_shr_col_grad | met_name), family = poisson(), data = top_centered %>% filter(met_name %in% top_50_2017))    


lmer4@frame %>% mutate(pred = predict(lmer4, type = 'response')) %>%  
  #filter(listing_count<10000, pred<10000) %>%
  #filter(listing_loc == 'New York City') %>% 
  ggplot(aes(pred, listing_count))+
  geom_point()+
  #scale_x_log10()+
  #scale_y_log10()+
  geom_abline()

test_frame <- lmer4@frame %>% mutate(pred = predict(lmer4, type = 'response'), offset = `offset(log(trt_tot_vac_hu_for_rent_1))`) %>% 
  gather(type, value, pred, listing_count)
  
test_frame %>% ggplot(aes(offset, trt_shr_wht))+geom_smooth(method = 'lm')+geom_abline()


ggplot(test_frame, aes(offset, value, color = trt_shr_wht, group = trt_shr_wht))+
  geom_point(alpha = .3)+
  facet_wrap(~type)

  
## look at effects using counterfactuals
# note that this will look different if you're not using one of the lmers
cf_base <- model@frame %>% summarise_if(is.numeric, mean) %>% mutate(join = 1, trt_tot_vac_hu_for_rent_1 = exp(`offset(log(trt_tot_vac_hu_for_rent_1))`))

race_mult <- cf_base %>% transmute(shr_race_total = trt_shr_blk+trt_shr_lat, blk_mult = trt_shr_blk/shr_race_total, lat_mult = trt_shr_lat/shr_race_total)

# you can change the specification of this to change the cfs produced
cfs <- tibble(
  #met_name = rep(ranef_tib %>% filter(abs(tot_wht_eff)<.35) %>% pull(met_name), each = 20),
  met_name = rep(focus_metros, each = 20),
  #trt_shr_col_grad = rep(rep(seq(-2,2,length.out = 20),20),7),
  trt_shr_wht = rep(seq(0,1, length.out = 20),length(focus_metros)),
  join = 1
) %>% inner_join(cf_base %>% select(-trt_shr_wht), by = 'join') %>% 
  mutate(
        trt_shr_lat = (0.9157739 - trt_shr_wht)*race_mult$lat_mult,
        trt_shr_blk = (0.9157739 - trt_shr_wht)*race_mult$blk_mult,
        trt_shr_tot_vac_hu_for_rent_1 = 100
        )

# change the model here
cfs <- cfs %>% mutate(pred = predict(model, newdata = cfs, type='response'))
max_min <- top_centered %>% group_by(met_name) %>% summarise(min = min(trt_shr_col_grad), max = max(trt_shr_col_grad))

# plot that stuff
cfs  %>% 
  #inner_join(max_min, by = 'met_name') %>% filter(trt_shr_col_grad<max, trt_shr_col_grad>min) %>%
  ggplot(aes(trt_shr_wht, pred))+
  #geom_point()+
  geom_line(size =1, alpha = .5)+
  facet_wrap(~met_name, scale = 'free')+
  labs(color = 'Tract Share White')+
  xlab("Tract Share College Grad, scaled")+
  ylab("Expected Count of Listings")+
  ggtitle('Expected Listings by Eudcation, White Share, and Selected Metro')

## Trying to focus in on effect of White increase

cf_base <- model@frame %>% summarise_if(is.numeric, mean) %>% mutate(join = 1, trt_tot_vac_hu_for_rent_1 = exp(`offset(log(trt_tot_vac_hu_for_rent_1))`))

race_mult <- cf_base %>% transmute(shr_race_total = trt_shr_blk+trt_shr_lat, blk_mult = trt_shr_blk/shr_race_total, lat_mult = trt_shr_lat/shr_race_total)
cfs <- tibble(
  met_name = rep(top_metros, each =2),
  trt_shr_wht = rep(c(.4,.5), 100),
  join = 1
) %>% inner_join(cf_base %>% select(-trt_shr_wht), by = 'join') %>% 
  mutate(
    trt_shr_lat = (0.9157739 - trt_shr_wht)*race_mult$lat_mult,
    trt_shr_blk = (0.9157739 - trt_shr_wht)*race_mult$blk_mult,
    trt_shr_tot_vac_hu_for_rent_1 = 100
  )


cfs <- cfs %>% mutate(pred = predict(model, newdata = cfs, type='response')) %>% select(met_name, trt_shr_wht, pred) %>% spread(trt_shr_wht, pred) %>% mutate(diff = `0.5`-`0.4`) 

cfs %>%
  filter(abs(diff)<.25) %>%
  ggplot(aes(fct_reorder(str_extract(met_name, '^[\\w\\.\\s]*'), diff), diff))+
  geom_hline(yintercept = 0, color = 'red')+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab('Metro Area')+
  ylab('Estimated effect of White Increase')


