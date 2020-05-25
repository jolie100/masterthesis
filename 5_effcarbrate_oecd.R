# set working directory
setwd("C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data")
dir()

# install.packages("extrafonts")

# load packages
library(dplyr)
library(xlsx)
library(readxl)
library(tidyverse)
library(wbstats) # to load WB GDP data
library(WDI)
library(naniar) # to plot missing values
library(extrafont)

# load data
carbpricegap_oecd <- read_excel("oecd2018_effective_carbon_rates.xlsx", sheet = "CarbonPricingGap_EUR60")
load("income_wb.Rdata")

glimpse(eff_carb_rate)

load("eff_carb_rate.Rdata")

################################################## Effective carbon rates for 33 of those 36 countries from carbonprice_wb

# add income groups
eff_carb_rate <- carbpricegap_oecd %>%
  select(country, iso, year, carbon_pricing_gap_60eur) %>%
  # filter(year == 2015) %>%
  left_join(income_wb, by = c("iso" = "Code")) %>%
  filter(incomegroup == "High income")

# delete cases with just NAs
eff_carb_rate <- eff_carb_rate[complete.cases(eff_carb_rate[ , "carbon_pricing_gap_60eur"]),]

# count number of countries in df
length(unique(eff_carb_rate$carbon_pricing_gap_60eur)) # 33 economies are included

# order df in effcarbrate ascending order to print table
eff_carb_rate <- eff_carb_rate %>%
  group_by(iso) %>%
  arrange(carbon_pricing_gap_60eur)

# safe table as LaTex file 
print(xtable(eff_carb_rate, caption = "Carbon pricing gap at a carbon price of 60???/tCO2", label = "eff_carb_rate", type = "latex"), file = "eff_carb_rate.tex")

# save df
save(eff_carb_rate,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/eff_carb_rate.Rdata")

################################################## box plot

# looking at distribution of effective carbon price gaps in 2015 

glimpse(eff_carb_rate)

eff_carb_rate_UK <- eff_carb_rate %>%
  filter(iso == "GBR")

# middle line: median of distribution
# edge bottom: 1st quartile
# edge top: 3 quartile
results_boxplot_effcarbrate2015 <- eff_carb_rate %>%
  # filter(date > 2002, date < 2020) %>% # , PopTotal > 35
  ggplot(aes(x = year, y = carbon_pricing_gap_60eur)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2) +
  geom_hline(data = eff_carb_rate_UK, aes(yintercept = carbon_pricing_gap_60eur), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  # scale_y_continuous(limits=c(0,300)) + 
  scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), legend.position = "bottom") +
  labs(title="Carbon pricing gap at a carbon price of 60???/tCO2",
       subtitle = "",
       x= "",
       y= "Carbon pricing gap at a carbon price of 60???/tCO2")

results_boxplot_effcarbrate2015 

ggsave("results_boxplot_effcarbrate2015.pdf")

################################################## bar plot


results_barplot_effcarbrate2015 <- eff_carb_rate %>%
  mutate(ToHighlight = ifelse(iso == "GBR", "GBR", "Others")) %>%
  filter(year == "2015") %>% # , PopTotal > 35
  ggplot(aes(x = reorder(iso, -carbon_pricing_gap_60eur), y = carbon_pricing_gap_60eur, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hline(data = eff_carb_rate_UK, aes(yintercept = carbon_pricing_gap_60eur), color = '#4DAF4A', size = 1) +
  coord_flip() +
  labs(fill = "") +
  scale_fill_manual(values = c("GBR" = "darkred", "Others" = "gray")) + 
  scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), 
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.background = element_blank()) +
  labs(# title="Carbon pricing gaps in 2015 to a carbon price of 60???/tCO2",
       subtitle = "",
       x= "",
       y= "Gap to a carbon price of 60???/tCO2")

results_barplot_effcarbrate2015 

ggsave("results_barplot_effcarbrate2015.pdf")

################################################## bar plot netherlands and gbr 2012 and 2015

library(extrafont)
font_import()
loadfonts(device = "win")

eff_carb_rate$carbon_pricing_gap_60eur <- as.numeric(eff_carb_rate$carbon_pricing_gap_60eur)

results_barplot_effcarbrate_NLD.UK <- eff_carb_rate %>%
  mutate(ToHighlight = ifelse(iso == "GBR", "GBR", "NLD")) %>%
  filter(iso %in% c("GBR", "NLD")) %>% # , PopTotal > 35
  group_by(iso) %>%
  ggplot(aes(x = reorder(year, -carbon_pricing_gap_60eur), y = carbon_pricing_gap_60eur, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity", position = "dodge") +
  # geom_hline(data = eff_carb_rate_UK, aes(yintercept = carbon_pricing_gap_60eur), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  labs(fill = "") + 
  scale_fill_manual(values = c("GBR" = "darkred", "NLD" = "gray")) + 
  # scale_color_manual(values = c("GBR" = "darkred", "NLD" = "gray")) +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete() +
  theme(panel.grid = element_blank(), 
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.background = element_blank(),
        legend.direction = "horizontal") +
  labs(x= "",
       y= "Gap to a carbon price of 60???/tCO2")

results_barplot_effcarbrate_NLD.UK 

ggsave("results_barplot_effcarbrate_NLD.UK.pdf", width = 10, height = 10, units = "cm")
