
library(tidyverse)
library(sf)
library(mgcv)
library(sandwich)
library(visreg)
library(stargazer)
library(effsize)

#working directory = repo base dir
setwd("R:/Project/natrent-city-sub")

#load model data
load("./output/extract/tract_mdata_post_id.RData")

#- bivariate maps
#- cohen's d analysis overall
#- cohen's d analysis city/suburb
#- linear models overall
#- linear models city/suburb
#- gam models overall
#- gam smooth term for vacancy
#- gam smooth term for poverty/shrblack


sum(tract_mdata$cl_listing_count[tract_mdata$platform=="Craigslist"])
sum(tract_mdata$apts_listing_count[tract_mdata$platform=="Apartments.com"])
length(unique(tract_mdata$trt_id))

#### Mutate a few columns prior to analysis

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
                                         NA, trt_med_yr_rent_hu_blt))


#### Listwise delete

vars_set <- c("trt_shr_col_grad", "trt_med_hh_inc", "trt_shr_wht",
              "trt_med_gross_rent", "trt_shr_col_stud", "trt_med_own_hu_val",
              "trt_shr_sfh", "trt_shr_eng_only", "trt_med_n_rooms", 
              "trt_shr_nonrel", "trt_shr_age_20_34", "trt_shr_male", 
              "trt_shr_age_65plus", "trt_shr_blt_pre_1940", "trt_pop_dens",
              "trt_shr_same_home", "trt_shr_for_born", "trt_avg_hh_size",
              "trt_shr_lat", "trt_shr_blk", "trt_shr_hcb", "trt_shr_pov",
              "dist_to_cbd")

tract_mdata <- tract_mdata %>%
  filter_at(vars(all_of(vars_set)), all_vars(!is.na(.))) 


#### Plot the distribution of lambda (as observed and transformed) ------------

## Distribution of lambda
ggplot(tract_mdata, aes(x = lambda, fill = platform, color = platform)) +
  geom_density(position = "identity", alpha = 0.5) +
  labs(fill = "Platform", color = "Platform", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/post_id/desc/observed.pdf",
         width = 6, height = 4, dpi = 300)

ggplot(tract_mdata, aes(x = lambda, fill = platform, color = platform)) +
  geom_density(aes(weight = trt_tot_rent_hu/met_tot_rent_hu), position = "identity", alpha = 0.5) +
  labs(fill = "Platform", color = "Platform", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/post_id/desc/observed_weighted.pdf",
         width = 6, height = 4, dpi = 300)

## Distribution of log(lambda)

ggplot(tract_mdata, aes(x = log(lambda), fill = platform, color = platform)) +
  geom_density(position = "identity", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(fill = "Platform", color = "Platform",
       x = "\nlog(Lambda)", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/post_id/desc/log(lambda).pdf",
         width = 6, height = 4, dpi = 300)

ggplot(tract_mdata, aes(x = log(lambda), fill = platform, color = platform)) +
  geom_density(aes(weight = trt_tot_rent_hu/met_tot_rent_hu), position = "identity", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(fill = "Platform", color = "Platform",
       x = "\nlog(Lambda)", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggsave(filename = "./output/post_id/desc/log(lambda)_weighted.pdf",
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
  ggsave(filename = "./output/post_id/desc/log(lambda)_by_city_sub.pdf",
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
  ggsave(filename = "./output/post_id/desc/log(lambda)_weighted_by_city_sub.pdf",
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
  ggsave(filename = "./output/post_id/desc/city_sub_rep.pdf",
         width = 6, height = 4, dpi = 300)


#### Difference in means for over and underrepresented tracts -----------------

#difference between lambda < 1 and lambda > 1 over pooled SD

#overall cohen's d analysis for each source
diff_in_means_overall <- tract_mdata %>%
  st_drop_geometry() %>%
  filter(!is.na(over_under)) %>%
  group_by(platform) %>%
  summarize_at(vars(all_of(vars_set)),
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
         variable = fct_reorder(variable, estimate))

#city/suburb cohen's d analysis for each source
diff_in_means_city_sub <- tract_mdata %>%
  st_drop_geometry() %>%
  filter(!is.na(over_under)) %>%
  group_by(platform, city_sub) %>%
  summarize_at(vars(all_of(vars_set)),
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

diff_in_means_city_sub$variable <- factor(diff_in_means_city_sub$variable,
                                       levels = levels(diff_in_means_overall$variable))

var_labels <- rev(c("College Degree", "Median HH Income", "Median Gross Rent",
                    "Median N Rooms", "Non-Latino White", "Single Family Home", "Median Owned HU Value",
                    "Speaking English Only", "Male", "College Student",  "Age 65+", "Nonrelatives in HH",
                    "Average HH Size", "Same Home Last Year", "Age 20-34",  "Distance to CBD", "Foreign Born",
                    "Population Density", "Latino", "HU Built Before 1940",  "Housing Cost Burdened", "Non-Latino Black",
                    "Poverty Rate"))

#overall plot
ggplot(diff_in_means_overall, 
       aes(x = estimate, y = variable, shape = platform, color = platform,
           xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_point(size = 2) +
  geom_errorbar(width = 0) +
  scale_x_continuous(limits = c(-.8, .8), 
                     breaks = c(-.8, -.5, -.2, 0, .2, .5, .8), 
                     minor_breaks = F) +
  #scale_y_discrete(labels = var_labels) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        panel.spacing.x = unit(.25, "in")) +
  labs(x = "\nCohen's d estimate", y = "", 
       color = "Platform", shape = "Platform") +
  ggsave(filename = "./output/post_id/desc/overall_diff_in_means.pdf",
         width = 8, height = 6, dpi = 300)

#city/suburb plot
ggplot(diff_in_means_city_sub, 
       aes(x = estimate, y = variable, shape = platform, color = platform,
           xmin = lower, xmax = upper)) +
  facet_grid(~ city_sub) +
  geom_vline(xintercept = 0, color = "grey60") +
  geom_point(size = 2) +
  geom_errorbar(width = 0) +
  scale_x_continuous(limits = c(-.8, .8), 
                     breaks = c(-.8, -.5, -.2, 0, .2, .5, .8),
                     minor_breaks = F) +
  #scale_y_discrete(labels = var_labels) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        panel.spacing.x = unit(.25, "in")) +
  labs(x = "\nCohen's d estimate", y = "", 
       color = "Platform", shape = "Platform") +
  ggsave(filename = "./output/post_id/desc/city_sub_diff_in_means.pdf",
         width = 8, height = 6, dpi = 300)


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
                               "Non-Latino Black", "Latino", "Non-Latino White", 
                               "log(Median HH Income) $\\times$ Non-Latino White"),
          notes.label = "Robust (HC3) Standard Errors in Parentheses",
          out = "./output/post_id/model/replication_models.tex")


#### GAM of differences in representation -------------------------------------

#Extension 1: 
gam_lambda_form_1 <- log(lambda) ~ 
  trt_tot_rent_hu + s(trt_vac_rate) +
  trt_shr_same_home + log(dist_to_cbd) + trt_shr_blt_pre_1940 +
  trt_med_n_rooms + trt_med_gross_rent + log(trt_med_hh_inc) +  trt_shr_age_20_34 + 
  trt_shr_age_65plus + trt_shr_col_stud + trt_shr_eng_only + 
  log(trt_avg_hh_size) + trt_shr_col_grad +
  trt_shr_blk + trt_shr_lat + 
  trt_shr_wht * log(trt_med_hh_inc) + met_id

#Extension 2: Metro segregation interactions with racial/ethnic differences
gam_lambda_form_2 <- log(lambda) ~ 
  trt_tot_rent_hu + s(trt_vac_rate) +
  trt_shr_same_home + log(dist_to_cbd) + trt_shr_blt_pre_1940 +
  trt_med_n_rooms + trt_med_gross_rent + log(trt_med_hh_inc) + trt_shr_age_20_34 + 
  trt_shr_age_65plus + trt_shr_col_stud + trt_shr_eng_only + 
  log(trt_avg_hh_size) + trt_shr_col_grad +
  trt_shr_blk + trt_shr_lat + 
  trt_shr_wht * log(trt_med_hh_inc) +
  I(dis_blk_wht * trt_shr_blk) + 
  I(dis_lat_wht * trt_shr_lat) + met_id


#estimate GAMs for each platform
gam_lambda_cl_1 <- gam(gam_lambda_form_1,
                       data = tract_mdata %>% 
                         filter(platform == "Craigslist"),
                       family = gaussian,
                       method = "REML",
                       select = TRUE)

gam_lambda_cl_2 <- gam(gam_lambda_form_2,
                       data = tract_mdata %>% 
                         filter(platform == "Craigslist"),
                       family = gaussian,
                       method = "REML",
                       select = TRUE)

gam_lambda_apts_1 <- gam(gam_lambda_form_1,
                         data = tract_mdata %>% 
                           filter(platform == "Apartments.com"),
                         family = gaussian,
                         method = "REML",
                         select = TRUE)

gam_lambda_apts_2 <- gam(gam_lambda_form_2,
                         data = tract_mdata %>% 
                           filter(platform == "Apartments.com"),
                         family = gaussian,
                         method = "REML",
                         select = TRUE)

#GAM summary tables
summary(gam_lambda_cl_1)
summary(gam_lambda_cl_2)
summary(gam_lambda_apts_1)
summary(gam_lambda_apts_2)

#GAM diagnostics
#gam.check(gam_lambda_cl)
#gam.check(gam_lambda_apts)

#simple plots for smooth term effects
#plot(gam_lambda_cl)
#plot(gam_lambda_apts)

#compare IC
BIC(rep_lm_lambda_cl)
BIC(gam_lambda_cl_1)
BIC(gam_lambda_cl_2)

BIC(rep_lm_lambda_apts)
BIC(gam_lambda_apts_1)
BIC(gam_lambda_apts_2)


#### Output GAM coefficient table ---------------------------------------------

stargazer::stargazer(gam_lambda_cl_1, gam_lambda_apts_1,
                     gam_lambda_cl_2, gam_lambda_apts_2,
                     omit = c("trt_vac_rate", "trt_med_yr_rent_hu_blt", "met_id"),
                     #covariate.labels = c("Total Rental HU", "\\% in Same Home Last Year",
                    #                      "log(Distance to CBD)", "\\% HU Built Before 1940", "Median N Rooms", "Median Gross Rent",
                    #                      "log(Median HH Income)", "\\% Age 20-34", "\\% Age 65+", "\\% College Student",
                    #                      "\\% Speaking English Only", "log(Average HH Size)", "\\% College Graduate",
                    #                      "\\% Non-Latino Black", "\\% Latino", "\\% Non-Latino White", 
                    #                      "Black-White Segregation", "Latino-White Segregation",
                    #                      "log(Median HH Income) $\\times$ \\% Non-Latino White",
                    #                      "Black-White Segregation $\\times$ \\% Non-Latino Black",
                    #                      "Latino-White Segregation $\\times$ \\% Latino"),
                     add.lines = list(c("BIC", 
                                        round(BIC(gam_lambda_cl_1),1), 
                                        round(BIC(gam_lambda_apts_1),1),
                                        round(BIC(gam_lambda_cl_2),1), 
                                        round(BIC(gam_lambda_apts_2),1)),
                                      c("Metro Fixed Effects?", rep("Yes", 4)),
                                      c("Includes Smooth for Vacancy $\\times$ Median Year Blt", rep("Yes", 4))),
                     column.separate = c(2, 2),
                     column.labels = c("Model 1", "Model 2"),
                     dep.var.caption = "",
                     keep.stat = c("n"),
                     style = "demography",
                     title = "Generalized Additive Models (GAM) of log(Lambda) for Craigslist and Apartments.com",
                     out = "./output/post_id/model/gam.tex")


## Overall smooth term plots --------------------------------------------------

### Vacancy X Median Year Built Tensor Product Smooth

#estimate linear predictions at different vacancy vals with other covariates at means
#lm_cl_plot <- visreg(rep_lm_lambda_cl, 
#                      xvar = "trt_vac_rate",
#                      plot = FALSE)$fit %>%
#  mutate(platform = "Craigslist", fit = "Linear")
#lm_apts_plot <- visreg(rep_lm_lambda_apts, 
#                        xvar = "trt_vac_rate", 
#                        plot = FALSE)$fit %>%
#  mutate(platform = "Apartments.com", fit = "Linear")

#estimate GAM predictions at different vacancy and med yr vals with other covariates at means
gam_cl_plot_vac <- visreg(gam_lambda_cl_2, 
                      xvar = "trt_vac_rate",
                   #   by = "trt_med_yr_rent_hu_blt",
                      plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline")

gam_apts_plot_vac <- visreg(gam_lambda_apts_2, 
                        xvar = "trt_vac_rate", 
                     #   by = "trt_med_yr_rent_hu_blt",
                        plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline")

#combined different platform's predictions into a tbl, trim to 10th/90th Ptiles of vac
gam_plot_vac <- bind_rows(gam_cl_plot_vac, gam_apts_plot_vac) %>%
  filter(trt_vac_rate >= 0,
         trt_vac_rate <= 0.15)

#plot the predictions, save to disk
ggplot(gam_plot_vac, aes(x = trt_vac_rate*100, y = visregFit, 
                    # color = as.factor(trt_med_yr_rent_hu_blt), 
                    # fill = as.factor(trt_med_yr_rent_hu_blt),
                    # group = as.factor(trt_med_yr_rent_hu_blt),
                     ymin = visregLwr, ymax = visregUpr)) +
  facet_grid(~ platform) +
  geom_line() +
  geom_ribbon(alpha = .5, color = NA) +
  scale_x_continuous(labels = function(x){paste0(x, "%")}) +
 # scale_color_brewer(palette = "Set1", 
#                     labels = c("1952 (10th Ptile)", "1974 (50th Ptile)", "1996 (90th Ptile)")) +
 ## scale_fill_brewer(palette = "Set1", 
  #                  labels = c("1952 (10th Ptile)", "1974 (50th Ptile)", "1996 (90th Ptile)")) +
  theme_minimal() +
  theme(panel.spacing = unit(.25, "in"),
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        legend.position = "bottom") +
  labs(x = "\nVacancy Rate", y = "Predicted log(Lambda)\n") +
  ggsave(filename = "./output/post_id/model/vac_rate_smoothed_effect.pdf",
         width = 8, height = 6, dpi = 300)


### Metropolitan segregation and % black interaction --------------------------

#predict log(lambda) at different % black and segregation values
gam_cl_plot_shr_blk <- visreg(gam_lambda_cl_2, 
                      xvar = "trt_shr_blk",
                      by = "dis_blk_wht",
                      plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline")
gam_apts_plot_shr_blk <- visreg(gam_lambda_apts_2, 
                        xvar = "trt_shr_blk", 
                        by = "dis_blk_wht",
                        plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline")

#combine different platform predictions into a single tbl, limit to 0-30%
gam_plot_shr_blk <- bind_rows(gam_cl_plot_shr_blk, gam_apts_plot_shr_blk) %>%
  filter(trt_shr_blk >= 0,
         trt_shr_blk <= .4)

#plot the predictions, save to disk
ggplot(gam_plot_shr_blk, aes(x = trt_shr_blk*100, y = visregFit, 
                     color = as.factor(dis_blk_wht), 
                     fill = as.factor(dis_blk_wht),
                     group = as.factor(dis_blk_wht),
                     ymin = visregLwr, ymax = visregUpr)) +
  facet_grid(~ platform) +
  geom_line() +
  geom_ribbon(alpha = .5, color = NA) +
  theme_minimal() +
  theme(panel.spacing = unit(.25, "in"),
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        legend.position = "bottom") +
  scale_x_continuous(labels = function(x){paste0(x, "%")}) +
  scale_color_brewer(palette = "Set1", labels = c(".47 (10th Ptile)", ".60 (50th Ptile)", ".75 (90 Ptile)")) +
  scale_fill_brewer(palette = "Set1", labels = c(".47 (10th Ptile)", ".60 (50th Ptile)", ".75 (90 Ptile)")) +
  labs(x = "\nShare Non-Latino Black", y = "Predicted log(Lambda)\n",
       color = "Black-White Segregation", fill = "Black-White Segregation") +
  ggsave(filename = "./output/post_id/model/shr_black_interaction_effect.pdf",
         width = 8, height = 6, dpi = 300)

### Metropolitan segregation and % Latino

#predict log lambda at different seg and % Latino values with other variables at means
gam_cl_plot_shr_lat <- visreg(gam_lambda_cl_2, 
                              xvar = "trt_shr_lat",
                              by = "dis_lat_wht",
                              plot = FALSE)$fit %>%
  mutate(platform = "Craigslist", fit = "Spline")
gam_apts_plot_shr_lat <- visreg(gam_lambda_apts_2, 
                                xvar = "trt_shr_lat", 
                                by = "dis_lat_wht",
                                plot = FALSE)$fit %>%
  mutate(platform = "Apartments.com", fit = "Spline")

#combine platform predictions into single tbl, limit % latino to 0-30%
gam_plot_shr_lat <- bind_rows(gam_cl_plot_shr_lat, gam_apts_plot_shr_lat) %>%
  filter(trt_shr_lat >= 0,
         trt_shr_lat <= .4)

#plot the predictions, save to disk
ggplot(gam_plot_shr_lat, aes(x = trt_shr_lat*100, y = visregFit, 
                             color = as.factor(dis_lat_wht), 
                             fill = as.factor(dis_lat_wht),
                             group = as.factor(dis_lat_wht),
                             ymin = visregLwr, ymax = visregUpr)) +
  facet_grid(~ platform) +
  geom_line() +
  geom_ribbon(alpha = .5, color = NA) +
  scale_x_continuous(labels = function(x){paste0(x, "%")}) +
  scale_color_brewer(palette = "Set1", labels = c(".38 (10th Ptile)", ".48 (50th Ptile)", ".59 (90 Ptile)")) +
  scale_fill_brewer(palette = "Set1", labels = c(".38 (10th Ptile)", ".48 (50th Ptile)", ".59 (90 Ptile)")) +
  theme_minimal() +
  theme(panel.spacing = unit(.25, "in"),
        plot.margin = unit(c(.25, .25, .25, .25), "in"),
        legend.position = "bottom") +
  labs(x = "\nShare Latino", y = "Predicted log(Lambda)\n",
       color = "Latino-White Segregation", fill = "Latino-White Segregation") +  
  ggsave(filename = "./output/post_id/model/shr_latino_interaction_effect.pdf",
         width = 8, height = 6, dpi = 300)


#### Appendix models ----------------------------------------------------------

#Appendix 1: adding smooth for vacancy 
gam_lambda_form_app_1 <- log(lambda) ~ 
  trt_tot_rent_hu + s(trt_vac_rate) +
  trt_shr_same_home + log(dist_to_cbd) + trt_shr_blt_pre_1940 +
  log(trt_med_hh_inc) + trt_med_n_rooms + trt_shr_age_20_34 + 
  trt_shr_age_65plus + trt_shr_col_stud + trt_shr_eng_only + 
  log(trt_avg_hh_size) + trt_shr_col_grad +
  trt_shr_blk + trt_shr_lat + trt_shr_wht * log(trt_med_hh_inc)

#Appendix 2: adding smooth for vacancy and median year built
gam_lambda_form_app_1 <- log(lambda) ~ 
  trt_tot_rent_hu + s(trt_vac_rate) +
  trt_shr_same_home + log(dist_to_cbd) + trt_shr_blt_pre_1940 +
  log(trt_med_hh_inc) + trt_med_n_rooms + trt_shr_age_20_34 + 
  trt_shr_age_65plus + trt_shr_col_stud + trt_shr_eng_only + 
  log(trt_avg_hh_size) + trt_shr_col_grad +
  trt_shr_blk + trt_shr_lat + trt_shr_wht * log(trt_med_hh_inc)


