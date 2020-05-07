
library(tidyverse)
library(sf)

#working directory = repo base dir
setwd("R:/Project/natrent-city-sub")

#load model data
load("./output/extract/tract_mdata_no_filter.RData")

tract_mdata_no_filter <- tract_mdata

load("./output/extract/tract_mdata_no_dedupe.RData")

tract_mdata_no_dedupe <- tract_mdata

load("./output/extract/tract_mdata_post_id.RData")

tract_mdata_post_id <- tract_mdata

load("./output/extract/tract_mdata_rent.RData")

tract_mdata_rent <- tract_mdata

load("./output/extract/tract_mdata.RData")

tract_mdata <- tract_mdata %>%
  mutate(def = "Bed X Sqft X Loc") 

tract_mdata_no_filter <- tract_mdata_no_filter %>%
  mutate(def = "No Filter, No Dedupe")

tract_mdata_no_dedupe <- tract_mdata_no_dedupe %>%
  mutate(def = "No Dedupe")

tract_mdata_post_id <- tract_mdata_post_id %>%
  filter(platform == "Craigslist") %>% # no apts.com equiv
  mutate(def = "Post ID")

tract_mdata_rent <- tract_mdata_rent %>%
  mutate(def = "Bed X Sqft X Loc X Rent")


compare_dedupe <- rbind(tract_mdata, tract_mdata_no_filter,
                        tract_mdata_no_dedupe, tract_mdata_post_id, 
                        tract_mdata_rent) %>%
  mutate(listings = ifelse(platform == "Craigslist", cl_listing_count, apts_listing_count))

compare_dedupe$def <- factor(compare_dedupe$def)
compare_dedupe$def <- factor(compare_dedupe$def, levels = rev(levels(compare_dedupe$def)[c(4, 3, 5, 2, 1)]))

#### Summary statistics



#### Graphics

ggplot(compare_dedupe, aes(x = log(lambda), color = def)) +
  facet_wrap(~ platform) +
  geom_freqpoly() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_freqpoly.pdf",
         width = 8, height = 6, dpi = 300)

ggplot(compare_dedupe, aes(x = log(lambda))) +
  facet_grid(platform ~ def, switch="y") +
  geom_histogram() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  scale_y_continuous(position = "right") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.spacing = unit(.2, "in")) +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_histogram.pdf",
         width = 10, height = 6, dpi = 300)

ggplot(compare_dedupe %>% filter(platform == "Craigslist"), aes(x = log(lambda), fill = trt_tot_vac_hu_for_rent == 0)) +
  facet_grid(trt_tot_vac_hu_for_rent == 0 ~ def, switch = "y") +
  geom_histogram() +
  geom_freqpoly() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  scale_y_continuous(position = "right") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.spacing = unit(.2, "in")) +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_zero_vac_for_rent_units_histogram.pdf",
         width = 10, height = 6, dpi = 300)


ggplot(compare_dedupe, aes(x = log(lambda), color = trt_tot_vac_hu_for_rent == 0)) +
  facet_grid(platform ~ def, switch = "y") +
  #geom_density(position = "dodge", adjust = 3, alpha = .50) +
  geom_freqpoly() +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  scale_y_continuous(position = "right") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.spacing = unit(.2, "in")) +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_zero_vac_for_rent_units_freqpoly.pdf",
         width = 10, height = 6, dpi = 300)

ggplot(compare_dedupe, aes(x = log(lambda))) +
  facet_grid(platform ~ def, switch="y") +
  geom_density(adjust = 2) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  scale_y_continuous(position = "right") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.spacing = unit(.2, "in")) +
  labs(x = "\nLog(Lambda)", y = "Density\n") +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_density.pdf",
         width = 10, height = 6, dpi = 300)

ggplot(compare_dedupe, aes(x = listings, fill = def, color = def)) +
  facet_grid(platform ~ def) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggsave(filename = "./output/dedupe_compare/n_dist_compare_histogram.pdf",
         width = 10, height = 6, dpi = 300)


ggplot(compare_dedupe, aes(x = trt_tot_vac_hu_for_rent, y = lambda, fill = def, color = def)) +
  facet_grid(platform ~ def) +
  geom_smooth() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggsave(filename = "./output/dedupe_compare/tot_vac_for_rent_by_lambda.pdf",
         width = 10, height = 6, dpi = 300)

ggplot()







ggplot(compare_dedupe, aes(x = log(lambda), fill = def, color = def)) +
  facet_wrap(~ def) +
  geom_histogram() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_histograms.pdf",
         width = 8, height = 6, dpi = 300)

ggplot(compare_dedupe, aes(x = log(lambda), color = def)) +
  geom_density(aes(weight = trt_tot_rent_hu/met_tot_rent_hu), alpha = .25) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  ggsave(filename = "./output/dedupe_compare/lambda_dist_compare_weighted.pdf",
         width = 8, height = 6, dpi = 300)

ggplot(compare_dedupe, aes(x = cl_listing_count, fill = def, color = def)) +
  facet_wrap(~ def) +
  scale_y_log10() +
  geom_histogram() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  ggsave(filename = "./output/dedupe_compare/cl_n_dist_compare.pdf",
         width = 8, height = 6, dpi = 300)


ggplot(compare_dedupe %>% filter(def != "Post ID"), aes(x = apts_listing_count, fill = def, color = def)) +
  facet_wrap(~ def) +
  scale_y_log10() +
  geom_histogram() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  ggsave(filename = "./output/dedupe_compare/apts_n_dist_compare.pdf",
         width = 8, height = 6, dpi = 300)






compare_dedupe <- compare_dedupe %>%
  group_by(trt_id) %>%
  mutate(diff_cl_listing = cl_listing_count[def=="No Filter"]-
           cl_listing_count[def=="Greedy"]) %>%
  filter(def == "Greedy")

cor.test(compare_dedupe$diff_cl_listing, compare_dedupe$trt_shr_blk)
cor.test(compare_dedupe$diff_cl_listing, compare_dedupe$lambda)

ggplot(compare_dedupe, aes(x = lambda, y = diff_cl_listing)) +
  geom_smooth()

ggplot(compare_dedupe %>% filter(lambda > 1), aes(x = lambda, y = diff_cl_listing)) +
  geom_smooth() 

ggplot(compare_dedupe %>% filter(lambda < 1), aes(x = lambda, y = diff_cl_listing)) +
  geom_smooth()












