#dependencies
library(tidyverse)
library(sf)
library(mgcv)
library(sandwich)
library(visreg)
library(xtable)
library(stargazer)
library(effsize)

#working directory = repo base dir
setwd("R:/Project/natrent-city-sub")

#load model data
load("./output/extract/tract.RData")

#load the summary data
load("./output/extract/sum.RData")

#set CRS for tract and tract_mdata
tract <- st_set_crs(tract, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
tract_mdata <- st_set_crs(tract_mdata, "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

#number of unique tracts
length(unique(tract_mdata$trt_id))


#### Results outline ----------------------------------------------------------

#- dedupe/filtering descriptions
#- bivariate maps
#- cohen's d analysis overall
#- cohen's d analysis city/suburb
#- linear models overall
#- linear models city/suburb
#- gam models overall
#- gam smooth term for vacancy
#- gam smooth term for poverty/shrblack


#### First create a table of summary statistics about sample between different data defs -----

## Craigslist

#restructure to data.frame, order by def
cl_sum <- bind_rows(cl_sum, .id = "def") 
cl_sum$def <- factor(cl_sum$def)
cl_sum$def <- factor(cl_sum$def, levels = levels(cl_sum$def)[c(4,3,5,2,1)])
cl_sum <- arrange(cl_sum, def)

#make a summary table with labels
cl_sum_tbl <- rbind(c("Listing Count", formatC(cl_sum$cl_listing_count, format = "d", big.mark = ",")), 
                    c("Average Rent", formatC(round(cl_sum$cl_avg_rent, 1), format = "d", big.mark = ",")), 
                    c("Median Rent", formatC(cl_sum$cl_med_rent, format = "d", big.mark = ",")), 
                    c("Average 2B Rent", formatC(cl_sum$cl_avg_rent_2b, format = "d", big.mark = ",")), 
                    c("Median 2B Rent", formatC(cl_sum$cl_med_rent_2b, format = "d", big.mark = ",")), 
                    c("Average Sqft", formatC(round(cl_sum$cl_avg_sqft, 0), format = "d", big.mark = ",")), 
                    c("Median Sqft", formatC(cl_sum$cl_med_sqft, format = "d", big.mark = ",")), 
                    c("Average Bedrooms", round(cl_sum$cl_avg_beds, 2)),
                    c("Median Bedrooms", cl_sum$cl_med_beds))
colnames(cl_sum_tbl) <- c("Statistic", as.character(cl_sum$def))

#convert to Latex and print to console
cl_sum_tbl <- xtable(cl_sum_tbl, 
                     caption = "Descriptive statistics for Craigslist data definitions")
print(cl_sum_tbl, include.rownames = FALSE, booktabs = TRUE,
      file = "./output/desc/cl_data_definitions.tex")

## Apartments.com

#restructure to data.frame, order by def
apts_sum <- bind_rows(apts_sum, .id = "def") 
apts_sum$def <- factor(apts_sum$def)
apts_sum <- arrange(apts_sum, desc(def))

#make a summary table with labels
apts_sum_tbl <- rbind(c("Listing Count", formatC(apts_sum$apts_listing_count, format = "d", big.mark = ",")), 
                      c("Average Rent", formatC(round(apts_sum$apts_avg_rent, 1), format = "d", big.mark = ",")), 
                      c("Median Rent", formatC(apts_sum$apts_med_rent, format = "d", big.mark = ",")), 
                      c("Average 2B Rent", formatC(apts_sum$apts_avg_rent_2b, format = "d", big.mark = ",")), 
                      c("Median 2B Rent", formatC(apts_sum$apts_med_rent_2b, format = "d", big.mark = ",")), 
                      c("Average Sqft", formatC(round(apts_sum$apts_avg_sqft, 0), format = "d", big.mark = ",")), 
                      c("Median Sqft", formatC(apts_sum$apts_med_sqft, format = "d", big.mark = ",")), 
                      c("Average Bedrooms", round(apts_sum$apts_avg_beds, 2)),
                      c("Median Bedrooms", apts_sum$apts_med_beds))
colnames(apts_sum_tbl) <- c("Statistic", as.character(apts_sum$def))

#convert to Latex and print to console
apts_sum_tbl <- xtable(apts_sum_tbl, 
                       caption = "Descriptive statistics for Apartments.com data definitions")
print(apts_sum_tbl, include.rownames = FALSE, booktabs = TRUE,
      file = "./output/desc/apts_data_definitions.tex")


#### Next, mutate a few columns prior to main analysis ------------------------

tract_mdata <- tract_mdata %>%
  mutate(trt_pop_dens = (trt_tot_pop/1000)/(aland/1000),
         trt_log_pop = log(trt_tot_pop),
         trt_med_hh_inc = trt_med_hh_inc/1000,
         trt_med_gross_rent = trt_med_gross_rent/1000,
         trt_med_own_hu_val = trt_med_own_hu_val/1000,
         trt_no_vac = trt_vac_rate == 0,
         city_sub = case_when(prin_city == "Y" ~ "City", 
                              prin_city != "Y" | is.na(prin_city) ~ "Suburb"),
         over_under  = case_when(lambda > 1 ~ "Overrepresented",
                                lambda < 1 ~ "Underrepresented"),
         trt_med_yr_rent_hu_blt = ifelse(trt_med_yr_rent_hu_blt <= 1600, 
                                         NA, trt_med_yr_rent_hu_blt)) %>%
  group_by(platform, met_id) %>%
  mutate(met_sub_rate = sum(trt_tot_pop[city_sub == "Suburb"])/sum(trt_tot_pop)) %>%
  ungroup()


#### Listwise delete ----------------------------------------------------------

vars_set <- c("trt_shr_col_grad", "trt_med_hh_inc", "trt_shr_wht",
              "trt_med_gross_rent", "trt_shr_col_stud", "trt_med_own_hu_val",
              "trt_shr_sfh", "trt_shr_eng_only", "trt_med_n_rooms", 
              "trt_shr_nonrel", "trt_shr_age_20_34", "trt_shr_male", 
              "trt_shr_age_65plus", "trt_shr_blt_pre_1940", "trt_pop_dens",
              "trt_shr_same_home", "trt_shr_for_born", "trt_avg_hh_size",
              "trt_shr_lat", "trt_shr_blk", "trt_shr_api", "trt_shr_aina",
              "trt_shr_oth", "trt_shr_hcb", "trt_shr_pov", "dist_to_cbd")

tract_mdata <- tract_mdata %>%
  filter_at(vars(all_of(vars_set)), all_vars(!is.na(.))) %>%
  filter(trt_tot_rent_hu > 0)


#### Summarize spatial compression and other distribution metrics -------------

zero_vac <- tract %>%
  filter(trt_id %in% tract_mdata$trt_id) %>%
  st_drop_geometry %>%
  group_by(def) %>%
  summarize(n_cl_listings = sum(cl_listing_count),
            n_apts_listings = sum(apts_listing_count),
            cl_gini = round(DescTools::Gini(cl_listing_count), 2),
            apts_gini = round(DescTools::Gini(apts_listing_count), 2),
            n_cl_zero_vac = sum(cl_listing_count[trt_tot_vac_hu_for_rent == 0]),
            n_apts_zero_vac = sum(apts_listing_count[trt_tot_vac_hu_for_rent == 0]),
            platform_dis = round((.5) * sum(abs(cl_listing_count/sum(cl_listing_count) - 
                                   apts_listing_count/sum(apts_listing_count))), 2),
            avg_cl_lambda_zero_vac = round(mean(cl_lambda[trt_tot_vac_hu_for_rent == 0])),
            avg_apts_lambda_zero_vac = round(mean(apts_lambda[trt_tot_vac_hu_for_rent == 0]))) %>%
  mutate(pct_cl_zero_vac = round(n_cl_zero_vac/n_cl_listings, 2),
         pct_apts_zero_vac = round(n_apts_zero_vac/n_apts_listings, 2)) %>%
  select(def, cl_gini, apts_gini, platform_dis, pct_cl_zero_vac, pct_apts_zero_vac,
         avg_cl_lambda_zero_vac, avg_apts_lambda_zero_vac)

zero_vac$def <- factor(zero_vac$def)
zero_vac$def <- factor(zero_vac$def, levels = levels(zero_vac$def)[c(4, 3, 5, 2, 1)])
zero_vac <- arrange(zero_vac, def) %>% t()
colnames(zero_vac) <- zero_vac[1,]
zero_vac <- zero_vac[-1,]
rownames(zero_vac) <- c("Craigslist Gini", "Apartments.com Gini",
                        "Platform Dissimilarity", 
                        "Prop. Craigslist in Zero Vacant For Rent Tract",
                        "Prop. Apartments.com in Zero Vacant For Rent Tract",
                        "Avg. Craigslist Lambda in Zero Vacant For Rent Tract",
                        "Avg. Apartments.com Lambda in Zero Vacant For Rent Tract")
zero_vac <- xtable(zero_vac, caption = "Spatial compression, platform segregation and representation of listings in zero vacancy tracts by data definition")
print(zero_vac, booktabs = TRUE, file = "./output/desc/distribution.tex")

#### Select preferred data definition for remaining analysis -----------------

tract_mdata <- tract_mdata %>% filter(def == "Beds X Sqft X Loc")

#### Plot the distribution of lambda (as observed and transformed) ------------

## Distribution of lambda
ggplot(tract_mdata, aes(x = lambda, fill = platform, color = platform)) +
  geom_density(position = "identity", alpha = 0.5) +
  labs(fill = "Platform", color = "Platform", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/desc/observed.pdf",
         width = 6, height = 4, dpi = 300)

ggplot(tract_mdata, aes(x = lambda, fill = platform, color = platform)) +
  geom_density(aes(weight = trt_tot_rent_hu/met_tot_rent_hu), position = "identity", alpha = 0.5) +
  labs(fill = "Platform", color = "Platform", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/desc/observed_weighted.pdf",
         width = 6, height = 4, dpi = 300)

## Distribution of log(lambda)

ggplot(tract_mdata, aes(x = log(lambda), fill = platform, color = platform)) +
  geom_density(position = "identity", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(fill = "Platform", color = "Platform",
       x = "\nlog(Lambda)", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/desc/log(lambda).pdf",
         width = 6, height = 4, dpi = 300)

ggplot(tract_mdata, aes(x = log(lambda), fill = platform, color = platform)) +
  geom_density(aes(weight = trt_tot_rent_hu/met_tot_rent_hu), position = "identity", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(fill = "Platform", color = "Platform",
       x = "\nlog(Lambda)", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/desc/log(lambda)_weighted.pdf",
         width = 6, height = 4, dpi = 300)

## Distribution of log(lambda) by city/suburb

ggplot(tract_mdata, aes(x = log(lambda), fill = platform, color = platform)) +
  facet_wrap(~ city_sub) +
  geom_density(position = "identity", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(fill = "Platform", color = "Platform",
       x = "\nlog(Lambda)", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/desc/log(lambda)_by_city_sub.pdf",
         width = 6, height = 4, dpi = 300)

ggplot(tract_mdata, aes(x = log(lambda), fill = platform, color = platform)) +
  facet_wrap(~ city_sub) +
  geom_density(aes(weight = trt_tot_rent_hu/met_tot_rent_hu),
                 position = "identity", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(fill = "Platform", color = "Platform",
       x = "\nlog(Lambda)", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/desc/log(lambda)_weighted_by_city_sub.pdf",
         width = 6, height = 4, dpi = 300)


## City/suburb representation across platforms --------------------------------

city_sub_rep <- tract_mdata %>%
  mutate(listing_count = ifelse(platform == "Craigslist",
                                cl_listing_count, 
                                apts_listing_count)) %>%
  st_drop_geometry() %>%
  group_by(platform, city_sub) %>%
  summarize(tot = sum(listing_count)) %>%
  mutate(shr = tot/sum(tot))

ggplot(city_sub_rep, aes(x = platform, y = shr, fill = city_sub)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Share of all listings\n", 
       fill = "Location") +
  ggsave(filename = "./output/desc/city_sub_rep.pdf",
         width = 6, height = 4, dpi = 300)


#### Difference in means for over and underrepresented tracts -----------------

#difference between lambda < 1 and lambda > 1 over pooled SD

#overall cohen's d analysis for each source
diff_in_means_overall <- tract_mdata %>%
  st_drop_geometry() %>%
  filter(!is.na(over_under)) %>%
  group_by(platform) %>%
  summarize_at(vars(all_of(vars_set), -trt_med_yr_rent_hu_blt),
               list(~ list(cohen.d(., f = over_under, pooled = T)))) %>%
  rowwise() %>%
  mutate_at(.vars = vars(-platform),
            .funs = list(estimate = function(x){x[["estimate"]]},
                         sd = function(x){x[['sd']]},
                         mag = function(x){as.character(x[['magnitude']])},
                         ci = function(x){list(x[['conf.int']])})) %>%
  gather(key = "key", value = "value", -platform) %>%
  filter(grepl("estimate|sd|ci|mag", key)) %>%
  mutate(variable = str_remove(key, pattern = "_estimate|_sd|_ci|_mag"),
         key = str_extract(key, pattern = "estimate|sd|ci|mag")) %>%
  spread(key = "key", value = "value") %>%
  rowwise() %>%
  mutate(estimate = estimate[[1]], sd = sd[[1]], lower = ci[[1]],
         upper = ci[[2]], mag = mag[[1]]) %>%
  ungroup() %>%
  mutate(variable = as_factor(variable),
         variable = fct_reorder(variable, estimate)) %>%
  mutate(city_sub = "Overall")

#city/suburb cohen's d analysis for each source
diff_in_means_city_sub <- tract_mdata %>%
  st_drop_geometry() %>%
  filter(!is.na(over_under)) %>%
  group_by(platform, city_sub) %>%
  summarize_at(vars(all_of(vars_set), -trt_med_yr_rent_hu_blt),
               list(~ list(cohen.d(., f = over_under, pooled = T)))) %>%
  rowwise() %>%
  mutate_at(.vars = vars(-platform, -city_sub),
            .funs = list(estimate = function(x){x[["estimate"]]},
                         sd = function(x){x[['sd']]},
                         mag = function(x){as.character(x[['magnitude']])},
                         ci = function(x){list(x[['conf.int']])})) %>%
  gather(key = "key", value = "value", -platform, -city_sub) %>%
  filter(grepl("estimate|sd|ci|mag", key)) %>%
  mutate(variable = str_remove(key, pattern = "_estimate|_sd|_ci|_mag"),
         key = str_extract(key, pattern = "estimate|sd|ci|mag")) %>%
  spread(key = "key", value = "value") %>%
  rowwise() %>%
  mutate(estimate = estimate[[1]], sd = sd[[1]], lower = ci[[1]],
         upper = ci[[2]], mag = mag[[1]]) %>%
  ungroup() %>%
  group_by(city_sub) %>%
  mutate(variable = as_factor(variable)) %>%
  ungroup()

diff_in_means <- bind_rows(diff_in_means_overall, diff_in_means_city_sub)

diff_in_means_city_sub$variable <- factor(diff_in_means_city_sub$variable,
                                       levels = levels(diff_in_means_overall$variable))

var_labels <- rev(c("Median HH Income", "College Degree", "Median Gross Rent",
                    "Median N Rooms", "NL White", "Single Family Home", 
                    "Median Owned HU Value", "Speaking English Only", "NL Asian/Pac. Isl.",
                    "NL Other Race" , "Male",  "Age 65+", "College Student", 
                    "Same Home Last Year", "Average HH Size", "Nonrelatives in HH",
                    "Distance to CBD", "NL Nat. Amer./Ala. Nat.",
                     "Age 20-34",   "Foreign Born", "Population Density", 
                    "HU Built Before 1940", "Latinx", "Housing Cost Burdened", "NL Black",
                    "Poverty Rate"))

diff_in_means$city_sub <- factor(diff_in_means$city_sub)
diff_in_means$city_sub <- factor(diff_in_means$city_sub, levels = levels(diff_in_means$city_sub)[c(2,1,3)])

#overall plot - unlabeled
ggplot(diff_in_means, 
       aes(x = estimate, y = variable, shape = platform, color = platform,
           xmin = lower, xmax = upper)) +
  facet_grid(~ city_sub) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_point(size = 2) +
  geom_errorbar(width = 0) +
  scale_x_continuous(limits = c(-.8, .8), 
                     breaks = c(-.8, -.5, -.2, .2, .5, .8),
                     minor_breaks = F) +
  #scale_y_discrete(labels = var_labels) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        panel.spacing.x = unit(.25, "in")) +
  labs(x = "\nCohen's d estimate", y = "", 
       color = "Platform", shape = "Platform") +
  ggsave(filename = "./output/desc/diff_in_means_unlabelled.pdf",
         width = 7, height = 4, dpi = 300)

ggplot(diff_in_means, 
       aes(x = estimate, y = variable, shape = platform, color = platform,
           xmin = lower, xmax = upper)) +
  facet_grid(~ city_sub) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_point(size = 2) +
  geom_errorbar(width = 0) +
  scale_x_continuous(limits = c(-.8, .8), 
                     breaks = c(-.8, -.5, -.2, .2, .5, .8),
                     minor_breaks = F) +
  scale_y_discrete(labels = var_labels) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        panel.spacing.x = unit(.25, "in"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8)) +
  labs(x = "\nCohen's d estimate", y = "", 
       color = "Platform", shape = "Platform") +
  ggsave(filename = "./output/desc/diff_in_means.pdf",
         width = 7, height = 4.5, dpi = 400)


#### Replication linear model estimation --------------------------------------

## replication linear model formula
rep_lm_lambda_form <- log(lambda) ~ 
  trt_tot_rent_hu + trt_vac_rate + trt_shr_same_home +
  log(dist_to_cbd) + trt_shr_blt_pre_1940 + trt_med_n_rooms + trt_med_gross_rent +
  log(trt_med_hh_inc) + trt_shr_age_20_34 + trt_shr_age_65plus +
  trt_shr_col_stud + trt_shr_eng_only + log(trt_avg_hh_size) +
  trt_shr_col_grad + trt_shr_blk + trt_shr_lat +
  trt_shr_wht * log(trt_med_hh_inc) + met_id

## Overall

#estimate linear models for each platform
rep_lm_lambda_cl <- lm(rep_lm_lambda_form,
                       data = tract_mdata %>%
                         filter(platform == "Craigslist"))

rep_lm_lambda_apts <- lm(rep_lm_lambda_form,
                         data = tract_mdata %>% 
                           filter(platform == "Apartments.com"))

## Cities

rep_lm_lambda_cl_city <- lm(rep_lm_lambda_form,   
                            data = tract_mdata %>% 
                              filter(platform == "Craigslist", 
                                     city_sub == "City"))

rep_lm_lambda_apts_city <- lm(rep_lm_lambda_form,
                              data = tract_mdata %>% 
                                filter(platform == "Apartments.com", 
                                       city_sub == "City"))

## Suburbs

rep_lm_lambda_cl_sub <- lm(rep_lm_lambda_form,
                           data = tract_mdata %>% 
                             filter(platform == "Craigslist", 
                                    city_sub == "Suburb"))

rep_lm_lambda_apts_sub <- lm(rep_lm_lambda_form,
                             data = tract_mdata %>% 
                               filter(platform == "Apartments.com", 
                                      city_sub == "Suburb"))

#summary of regression coefficients
summary(rep_lm_lambda_cl)
summary(rep_lm_lambda_apts)
summary(rep_lm_lambda_cl_city)
summary(rep_lm_lambda_apts_city)
summary(rep_lm_lambda_cl_sub)
summary(rep_lm_lambda_apts_sub)

#summary of regression coefficients with HC SEs
lmtest::coeftest(rep_lm_lambda_cl, vcov = vcovHC(rep_lm_lambda_cl))
lmtest::coeftest(rep_lm_lambda_apts, vcov = vcovHC(rep_lm_lambda_apts))
lmtest::coeftest(rep_lm_lambda_cl_city, vcov = vcovHC(rep_lm_lambda_cl_city))
lmtest::coeftest(rep_lm_lambda_apts_city, vcov = vcovHC(rep_lm_lambda_apts_city))
lmtest::coeftest(rep_lm_lambda_cl_sub, vcov = vcovHC(rep_lm_lambda_cl_sub))
lmtest::coeftest(rep_lm_lambda_apts_sub, vcov = vcovHC(rep_lm_lambda_apts_sub))

#plot residuals (seems there are some outliers with leverage)
#plot(lm_lambda_cl)
#plot(lm_lambda_apts)


#### Output replication model table for latex ---------------------------------

stargazer(rep_lm_lambda_cl, rep_lm_lambda_apts, 
          rep_lm_lambda_cl_city, rep_lm_lambda_apts_city, 
          rep_lm_lambda_cl_sub, rep_lm_lambda_apts_sub,
          se = list(sqrt(diag(vcovHC(rep_lm_lambda_cl))),
                    sqrt(diag(vcovHC(rep_lm_lambda_apts))),
                    sqrt(diag(vcovHC(rep_lm_lambda_cl_city))),
                    sqrt(diag(vcovHC(rep_lm_lambda_apts_city))),
                    sqrt(diag(vcovHC(rep_lm_lambda_cl_sub))),
                    sqrt(diag(vcovHC(rep_lm_lambda_apts_sub)))),
          omit = "met_id",
          keep.stat = c("n", "rsq"),
          add.lines=list(c("BIC", 
                           round(BIC(rep_lm_lambda_cl),1), 
                           round(BIC(rep_lm_lambda_apts),1),
                           round(BIC(rep_lm_lambda_cl_city),1), 
                           round(BIC(rep_lm_lambda_apts_city),1),
                           round(BIC(rep_lm_lambda_cl_sub),1), 
                           round(BIC(rep_lm_lambda_apts_sub),1)),
                         c("Metro Fixed Effects?", rep("Yes", 6))),
          column.separate = c(2, 2, 2),
          column.labels = c("Overall", "City", "Suburb"),
          style = "demography",
          title = "Boeing 2020 Environment and Planning: A Replication Models",
          covariate.labels = c("Total Rental HU", "Vacancy Rate", "Same Home Last Year",
                               "log(Distance to CBD)", "HU Built Before 1940", "Median N Rooms", "Median Gross Rent",
                               "log(Median HH Income)", "Age 20-34", "Age 65+", "College Student",
                               "Speaking English Only", "log(Average HH Size)", "College Graduate",
                               "Non-Latinx Black", "Latinx", "Non-Latinx White", 
                               "log(Median HH Income) $\\times$ Non-Latinx White"),
          notes.label = "Robust (HC3) Standard Errors in Parentheses",
          out = "./output/model/replication_models.tex")


#### extension linear model estimation ---------------------------------------------------

ext_lm_lambda_form <- log(lambda) ~ 
  trt_shr_blk + trt_shr_lat + trt_shr_api + trt_shr_aina + trt_shr_oth + 
  trt_shr_pov + trt_shr_col_grad + log(trt_med_hh_inc) + 
  trt_tot_rent_hu + trt_vac_rate + trt_shr_blt_pre_1940 + 
  trt_med_n_rooms + trt_med_gross_rent +
  trt_shr_same_home + log(dist_to_cbd) +
  trt_shr_age_20_34 + trt_shr_age_65plus + trt_shr_col_stud + trt_shr_eng_only + 
  log(trt_avg_hh_size)  


## Overall

#estimate linear models for each platform
ext_lm_lambda_cl <- lm(ext_lm_lambda_form,
                       data = tract_mdata %>%
                         filter(platform == "Craigslist"))

ext_lm_lambda_apts <- lm(ext_lm_lambda_form,
                         data = tract_mdata %>% 
                           filter(platform == "Apartments.com"))

## Cities

ext_lm_lambda_cl_city <- lm(ext_lm_lambda_form,   
                            data = tract_mdata %>% 
                              filter(platform == "Craigslist", 
                                     city_sub == "City"))

ext_lm_lambda_apts_city <- lm(ext_lm_lambda_form,
                              data = tract_mdata %>% 
                                filter(platform == "Apartments.com", 
                                       city_sub == "City"))

## Suburbs

ext_lm_lambda_cl_sub <- lm(ext_lm_lambda_form,
                           data = tract_mdata %>% 
                             filter(platform == "Craigslist", 
                                    city_sub == "Suburb"))

ext_lm_lambda_apts_sub <- lm(ext_lm_lambda_form,
                             data = tract_mdata %>% 
                               filter(platform == "Apartments.com", 
                                      city_sub == "Suburb"))

#summary of regression coefficients
summary(ext_lm_lambda_cl)
summary(ext_lm_lambda_apts)
summary(ext_lm_lambda_cl_city)
summary(ext_lm_lambda_apts_city)
summary(ext_lm_lambda_cl_sub)
summary(ext_lm_lambda_apts_sub)

#summary of regression coefficients with HC SEs
lmtest::coeftest(ext_lm_lambda_cl, vcov = vcovHC(ext_lm_lambda_cl))
lmtest::coeftest(ext_lm_lambda_apts, vcov = vcovHC(ext_lm_lambda_apts))
lmtest::coeftest(ext_lm_lambda_cl_city, vcov = vcovHC(ext_lm_lambda_cl_city))
lmtest::coeftest(ext_lm_lambda_apts_city, vcov = vcovHC(ext_lm_lambda_apts_city))
lmtest::coeftest(ext_lm_lambda_cl_sub, vcov = vcovHC(ext_lm_lambda_cl_sub))
lmtest::coeftest(ext_lm_lambda_apts_sub, vcov = vcovHC(ext_lm_lambda_apts_sub))

#plot residuals (seems there are some outliers with leverage)
#plot(lm_lambda_cl)
#plot(lm_lambda_apts)


#### Output extension model table for latex ---------------------------------

stargazer(ext_lm_lambda_cl, ext_lm_lambda_apts, 
          ext_lm_lambda_cl_city, ext_lm_lambda_apts_city, 
          ext_lm_lambda_cl_sub, ext_lm_lambda_apts_sub,
          se = list(sqrt(diag(vcovHC(ext_lm_lambda_cl))),
                    sqrt(diag(vcovHC(ext_lm_lambda_apts))),
                    sqrt(diag(vcovHC(ext_lm_lambda_cl_city))),
                    sqrt(diag(vcovHC(ext_lm_lambda_apts_city))),
                    sqrt(diag(vcovHC(ext_lm_lambda_cl_sub))),
                    sqrt(diag(vcovHC(ext_lm_lambda_apts_sub)))),
          omit = "met_id",
          keep.stat = c("n", "rsq"),
          add.lines=list(c("BIC", 
                           round(BIC(ext_lm_lambda_cl),1), 
                           round(BIC(ext_lm_lambda_apts),1),
                           round(BIC(ext_lm_lambda_cl_city),1), 
                           round(BIC(ext_lm_lambda_apts_city),1),
                           round(BIC(ext_lm_lambda_cl_sub),1), 
                           round(BIC(ext_lm_lambda_apts_sub),1))),
          column.separate = c(2, 2, 2),
          column.labels = c("Overall", "City", "Suburb"),
          style = "demography",
          title = "Linear regressions of log(Lambda) for Craigslist and Apartments.com",
          covariate.labels = c("Non-Latinx Black", "Latinx", "Non-Latinx Asian/Pac. Islander",
                               "Non-Latinx Native American/Alaska Native", "Non-Latinx Other Race",
                               "Poverty Rate", "College Degree", "log(Median HH Income)", 
                               "Total Rental HU", "Vacancy Rate", "Built Before 1940", 
                               "Median N Rooms", "Median Gross Rent", "Same Home Last Year",
                               "log(Distance to CBD)", "Age 20-34", "Age 65+", "College Student",
                               "English Speaking Only", "log(Average HH Size)"),
          notes.label = "Robust (HC3) Standard Errors in Parentheses",
          out = "./output/model/extension_models.tex")

#### Segregation model for differences in representation -------------------------------------

seg_lambda_form <- log(lambda) ~ 
  trt_shr_blk + trt_shr_lat + trt_shr_api + trt_shr_aina + trt_shr_oth + 
  trt_shr_pov + trt_shr_col_grad + log(trt_med_hh_inc) + 
  trt_tot_rent_hu + trt_vac_rate + trt_shr_blt_pre_1940 + 
  trt_med_n_rooms + trt_med_gross_rent +
  trt_shr_same_home + log(dist_to_cbd) +
  trt_shr_age_20_34 + trt_shr_age_65plus + trt_shr_col_stud + trt_shr_eng_only + 
  log(trt_avg_hh_size) +
  dis_blk_wht + dis_lat_wht + dis_poor_nonpoor +
  I(dis_blk_wht * trt_shr_blk) +
  I(dis_lat_wht * trt_shr_lat) + 
  I(dis_poor_nonpoor * trt_shr_pov) +
  met_shr_blk + met_shr_lat + met_shr_api + met_shr_aina + met_shr_oth + 
  met_shr_pov + met_shr_col_grad + log(met_med_hh_inc) + 
  met_tot_rent_hu + met_vac_rate + met_shr_blt_pre_1940 + 
  met_med_n_rooms + met_med_gross_rent +
  met_shr_same_home + met_shr_age_20_34 + met_shr_age_65plus +
  met_shr_col_stud + met_shr_eng_only + log(met_avg_hh_size)

#estimate models for each platform
seg_lm_lambda_cl <- lm(seg_lambda_form,
                     data = tract_mdata %>% 
                       filter(platform == "Craigslist"))

seg_lm_lambda_apts <- lm(seg_lambda_form,
                      data = tract_mdata %>% 
                        filter(platform == "Apartments.com"))

seg_lm_lambda_cl_city <- lm(seg_lambda_form,
                    data = tract_mdata %>% 
                      filter(platform == "Craigslist",
                             city_sub == "City"))

seg_lm_lambda_apts_city <- lm(seg_lambda_form,
                      data = tract_mdata %>% 
                        filter(platform == "Apartments.com",
                               city_sub == "City"))

seg_lm_lambda_cl_sub <- lm(seg_lambda_form,
                    data = tract_mdata %>% 
                      filter(platform == "Craigslist",
                             city_sub == "Suburb"))

seg_lm_lambda_apts_sub <- lm(seg_lambda_form,
                      data = tract_mdata %>% 
                        filter(platform == "Apartments.com",
                               city_sub == "Suburb"))

#summary of regression coefficients
summary(seg_lm_lambda_cl)
summary(seg_lm_lambda_apts)
summary(seg_lm_lambda_cl_city)
summary(seg_lm_lambda_apts_city)
summary(seg_lm_lambda_cl_sub)
summary(seg_lm_lambda_apts_sub)

#summary of regression coefficients with HC SEs
lmtest::coeftest(seg_lm_lambda_cl, vcov = vcovHC(seg_lm_lambda_cl))
lmtest::coeftest(seg_lm_lambda_apts, vcov = vcovHC(seg_lm_lambda_apts))
lmtest::coeftest(seg_lm_lambda_cl_city, vcov = vcovHC(seg_lm_lambda_cl_city))
lmtest::coeftest(seg_lm_lambda_apts_city, vcov = vcovHC(seg_lm_lambda_apts_city))
lmtest::coeftest(seg_lm_lambda_cl_sub, vcov = vcovHC(seg_lm_lambda_cl_sub))
lmtest::coeftest(seg_lm_lambda_apts_sub, vcov = vcovHC(seg_lm_lambda_apts_sub))

#### Output segregation model table for latex ---------------------------------

stargazer(seg_lm_lambda_cl, seg_lm_lambda_apts, 
          seg_lm_lambda_cl_city, seg_lm_lambda_apts_city, 
          seg_lm_lambda_cl_sub, seg_lm_lambda_apts_sub,
          se = list(sqrt(diag(vcovHC(seg_lm_lambda_cl))),
                    sqrt(diag(vcovHC(seg_lm_lambda_apts))),
                    sqrt(diag(vcovHC(seg_lm_lambda_cl_city))),
                    sqrt(diag(vcovHC(seg_lm_lambda_apts_city))),
                    sqrt(diag(vcovHC(seg_lm_lambda_cl_sub))),
                    sqrt(diag(vcovHC(seg_lm_lambda_apts_sub)))),
          omit = "met_id",
          keep.stat = c("n", "rsq"),
          add.lines=list(c("BIC", 
                           round(BIC(seg_lm_lambda_cl),1), 
                           round(BIC(seg_lm_lambda_apts),1),
                           round(BIC(seg_lm_lambda_cl_city),1), 
                           round(BIC(seg_lm_lambda_apts_city),1),
                           round(BIC(seg_lm_lambda_cl_sub),1), 
                           round(BIC(seg_lm_lambda_apts_sub),1))),
          column.separate = c(2, 2, 2),
          column.labels = c("Overall", "City", "Suburb"),
          style = "demography",
          title = "Linear models of segregation interaction with racial/ethnic composition",
          covariate.labels = c("Non-Latinx Black", "Latinx", "Non-Latinx Asian/Pac. Islander",
                               "Non-Latinx Native American/Alaska Native", "Non-Latinx Other Race",
                               "Poverty Rate", "College Degree", "log(Median HH Income)", 
                               "Total Rental HU", "Vacancy Rate", "Built Before 1940", 
                               "Median N Rooms", "Median Gross Rent", "Same Home Last Year",
                               "log(Distance to CBD)", "Age 20-34", "Age 65+", "College Student",
                               "English Speaking Only", "log(Average HH Size)",
                               "Black-White Segregation", "Latino White Segregation", "Poverty Segregation",
                               "Black-White Segregation \times NL Black",
                               "Latinx-White Segregation \times Latinx",
                               "Poverty Segregation \times Poverty Rate",
                               "Metro NL Black", "Metro Latinx", "Metro NL Asian/Pac. Islander",
                               "Metro NL Native American/Alaska Native", "Metro NL Other Race",
                               "Metro Poverty Rate", "Metro College Degree", "Metro log(Median HH Income)",
                               "Metro Total Rental HU", "Metro Vacancy Rate", "Metro Built Before 1940",
                               "Metro Median N Rooms", "Metro Median Gross Rent", "Metro Same Home Last Year",
                               "Metro Age 20-34", "Metro Age 65+", "Metro College Student", "Metro English Speaking Only",
                               "Metro log(Average HH Size)"),
          float.env = "table",
          notes.label = "Robust (HC3) Standard Errors in Parentheses",
          out = "./output/model/segregation_models.tex")

#### Output segregation model predictions --------------------------------------

#predict lambda at different % black and segregation values
cl_plot_shr_blk <- visreg(seg_lm_lambda_cl, 
                               xvar = "trt_shr_blk",
                               by = "dis_blk_wht",
                               breaks = c(.47, .60, .75), 
                               trans = exp,
                               plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "Overall")
apts_plot_shr_blk <- visreg(seg_lm_lambda_apts, 
                                 xvar = "trt_shr_blk", 
                                 by = "dis_blk_wht",
                                 breaks = c(.47, .60, .75),
                                 trans = exp,
                                 plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "Overall")

cl_plot_shr_blk_city <- visreg(seg_lm_lambda_cl_city, 
                                   xvar = "trt_shr_blk",
                                   by = "dis_blk_wht",
                                   breaks = c(.47, .60, .75),
                                   trans = exp,
                                   plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "City")
apts_plot_shr_blk_city <- visreg(seg_lm_lambda_apts_city, 
                                 xvar = "trt_shr_blk", 
                                 by = "dis_blk_wht",
                                 breaks = c(.47, .60, .75), 
                                 trans = exp,
                                 plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "City")

cl_plot_shr_blk_sub <- visreg(seg_lm_lambda_cl_sub, 
                              xvar = "trt_shr_blk",
                              by = "dis_blk_wht",
                              breaks = c(.47, .60, .75),
                              trans = exp,
                              plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "Suburb")
apts_plot_shr_blk_sub <- visreg(seg_lm_lambda_apts_sub, 
                                xvar = "trt_shr_blk", 
                                by = "dis_blk_wht",
                                breaks = c(.47, .60, .75),
                                trans = exp,
                                plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "Suburb")

#combine different platform predictions into a single tbl, limit to 0-30%
plot_shr_blk <- bind_rows(cl_plot_shr_blk,apts_plot_shr_blk,
                          cl_plot_shr_blk_city, apts_plot_shr_blk_city,
                              cl_plot_shr_blk_sub, apts_plot_shr_blk_sub)  %>%
  filter(trt_shr_blk >= 0,
         trt_shr_blk <= .5)
plot_shr_blk$city_sub <- factor(plot_shr_blk$city_sub)
plot_shr_blk$city_sub <- factor(plot_shr_blk$city_sub, levels = levels(plot_shr_blk$city_sub)[c(2, 1, 3)])

#plot the predictions, save to disk
ggplot(plot_shr_blk, aes(x = trt_shr_blk*100, y = visregFit, 
                             color = as.factor(dis_blk_wht), 
                             fill = as.factor(dis_blk_wht),
                             group = as.factor(dis_blk_wht),
                             ymin = visregLwr, ymax = visregUpr)) +
  facet_grid(platform ~ city_sub) +
  geom_line() +
  geom_ribbon(alpha = .25, color = NA) +
  theme_minimal() +
  theme(panel.spacing = unit(.25, "in"),
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        legend.position = "bottom") +
  scale_x_continuous(labels = function(x){paste0(x, "%")}) +
  scale_color_brewer(palette = "Set1", labels = c(".47 (10th Ptile)", ".60 (50th Ptile)", ".75 (90 Ptile)")) +
  scale_fill_brewer(palette = "Set1", labels = c(".47 (10th Ptile)", ".60 (50th Ptile)", ".75 (90 Ptile)")) +
  labs(x = "\nShare Non-Latinx Black", y = "Predicted Lambda\n",
       color = "Black-White Segregation", fill = "Black-White Segregation") +
  ggsave(filename = "./output/model/shr_black_interaction_effect_city_sub.pdf",
         width = 7, height = 4, dpi = 300)



#predict log(lambda) at different % Latino and segregation values
cl_plot_shr_lat <- visreg(seg_lm_lambda_cl, 
                          xvar = "trt_shr_lat",
                          by = "dis_lat_wht",
                          breaks = c(.38, .48, .59),
                          trans = exp,
                          plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "Overall")
apts_plot_shr_lat <- visreg(seg_lm_lambda_apts, 
                            xvar = "trt_shr_lat", 
                            by = "dis_lat_wht",
                            breaks = c(.38, .48, .59),
                            trans = exp,
                            plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "Overall")

cl_plot_shr_lat_city <- visreg(seg_lm_lambda_cl_city, 
                               xvar = "trt_shr_lat",
                               by = "dis_lat_wht",
                               breaks = c(.38, .48, .59),
                               trans = exp,
                               plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "City")
apts_plot_shr_lat_city <- visreg(seg_lm_lambda_apts_city, 
                                 xvar = "trt_shr_lat", 
                                 by = "dis_lat_wht",
                                 breaks = c(.38, .48, .59),
                                 trans = exp,
                                 plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "City")

cl_plot_shr_lat_sub <- visreg(seg_lm_lambda_cl_sub, 
                              xvar = "trt_shr_lat",
                              by = "dis_lat_wht",
                              breaks = c(.38, .48, .59),
                              trans = exp,
                              plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "Suburb")
apts_plot_shr_lat_sub <- visreg(seg_lm_lambda_apts_sub, 
                                xvar = "trt_shr_lat", 
                                by = "dis_lat_wht",
                                breaks = c(.38, .48, .59),
                                trans = exp,
                                plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "Suburb")

#combine different platform predictions into a single tbl, limit to 0-30%
plot_shr_lat <- bind_rows(cl_plot_shr_lat, apts_plot_shr_lat,
                          cl_plot_shr_lat_city, apts_plot_shr_lat_city,
                          cl_plot_shr_lat_sub, apts_plot_shr_lat_sub) %>%
  filter(trt_shr_lat >= 0,
         trt_shr_lat <= .5)
plot_shr_lat$city_sub <- factor(plot_shr_lat$city_sub)
plot_shr_lat$city_sub <- factor(plot_shr_lat$city_sub, levels = levels(plot_shr_lat$city_sub)[c(2, 1, 3)])

#plot the predictions, save to disk
ggplot(plot_shr_lat, aes(x = trt_shr_lat*100, y = visregFit, 
                         color = as.factor(dis_lat_wht), 
                         fill = as.factor(dis_lat_wht),
                         group = as.factor(dis_lat_wht),
                         ymin = visregLwr, ymax = visregUpr)) +
  facet_grid(platform ~ city_sub) +
  geom_line() +
  geom_ribbon(alpha = .25, color = NA) +
  theme_minimal() +
  theme(panel.spacing = unit(.25, "in"),
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        legend.position = "bottom") +
  scale_x_continuous(labels = function(x){paste0(x, "%")}) +
  scale_color_brewer(palette = "Set1", labels = c(".38 (10th Ptile)", ".48 (50th Ptile)", ".59 (90 Ptile)")) +
  scale_fill_brewer(palette = "Set1", labels = c(".38 (10th Ptile)", ".48 (50th Ptile)", ".59 (90 Ptile)")) +
  labs(x = "\nShare Latinx", y = "Predicted Lambda\n",
       color = "Latinx-White Segregation", fill = "Latinx-White Segregation") +
  ggsave(filename = "./output/model/shr_latinx_interaction_effect_city_sub.pdf",
         width = 7, height = 4, dpi = 300)




#predict lambda at different % poverty and segregation values
cl_plot_shr_pov <- visreg(seg_lm_lambda_cl, 
                          xvar = "trt_shr_pov",
                          by = "dis_poor_nonpoor",
                          breaks = c(.30, .35, .40),
                          trans = exp,
                          plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "Overall")
apts_plot_shr_pov <- visreg(seg_lm_lambda_apts, 
                            xvar = "trt_shr_pov", 
                            by = "dis_poor_nonpoor",
                            breaks = c(.30, .35, .40),
                            trans = exp,
                            plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "Overall")

cl_plot_shr_pov_city <- visreg(seg_lm_lambda_cl_city, 
                               xvar = "trt_shr_pov",
                               by = "dis_poor_nonpoor",
                               breaks = c(.30, .35, .40),
                               trans = exp,
                               plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "City")
apts_plot_shr_pov_city <- visreg(seg_lm_lambda_apts_city, 
                                 xvar = "trt_shr_pov", 
                                 by = "dis_poor_nonpoor",
                                 breaks = c(.30, .35, .40),
                                 trans = exp,
                                 plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "City")

cl_plot_shr_pov_sub <- visreg(seg_lm_lambda_cl_sub, 
                              xvar = "trt_shr_pov",
                              by = "dis_poor_nonpoor",
                              breaks = c(.30, .35, .40),
                              trans = exp,
                              plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline", city_sub = "Suburb")
apts_plot_shr_pov_sub <- visreg(seg_lm_lambda_apts_sub, 
                                xvar = "trt_shr_pov", 
                                by = "dis_poor_nonpoor",
                                breaks = c(.30, .35, .40),
                                trans = exp,
                                plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline", city_sub = "Suburb")

#combine different platform predictions into a single tbl, limit to 0-30%
plot_shr_pov <- bind_rows(cl_plot_shr_pov, apts_plot_shr_pov,
                          cl_plot_shr_pov_city, apts_plot_shr_pov_city,
                          cl_plot_shr_pov_sub, apts_plot_shr_pov_sub) %>%
  filter(trt_shr_pov >= 0,
         trt_shr_pov <= .5)
plot_shr_pov$city_sub <- factor(plot_shr_pov$city_sub)
plot_shr_pov$city_sub <- factor(plot_shr_pov$city_sub, levels = levels(plot_shr_pov$city_sub)[c(2, 1, 3)])

#plot the predictions, save to disk
ggplot(plot_shr_pov, aes(x = trt_shr_pov*100, y = visregFit, 
                         color = as.factor(dis_poor_nonpoor), 
                         fill = as.factor(dis_poor_nonpoor),
                         group = as.factor(dis_poor_nonpoor),
                         ymin = visregLwr, ymax = visregUpr)) +
  facet_grid(platform ~ city_sub) +
  geom_line() +
  geom_ribbon(alpha = .25, color = NA) +
  theme_minimal() +
  theme(panel.spacing = unit(.25, "in"),
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        legend.position = "bottom") +
  scale_x_continuous(labels = function(x){paste0(x, "%")}) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "\nPoverty Rate", y = "Predicted Lambda\n", 
       fill = "Poverty Segregation", color = "Poverty Segregation") +
  ggsave(filename = "./output/model/shr_pov_interaction_effect_city_sub.pdf",
         width = 7, height = 4, dpi = 300)



