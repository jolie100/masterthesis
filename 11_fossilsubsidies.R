# set working directory
setwd("C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data")
dir()

# load packages
library(dplyr)
library(xlsx)
library(readxl)
library(tidyverse)
library(wbstats) # to load WB GDP data
library(WDI)
library(naniar) # to plot missing values

# load data
fossil_subsidies <- read_excel("all_data.xlsx", sheet = "data")
load("income_wb.Rdata")
load("totpop.Rdata")
# income_wb <- read_excel("wb2019income.xls", sheet = "income")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name)) 

load("fossil_subsidies.Rdata")

################################################## create fossil_subsidies 

# create df (fos.sub in USD billions (annual))

glimpse(fossil_subsidies)

fossil_subsidies <- fossil_subsidies %>%
  filter(year > 2004) %>%
  left_join(income_wb, by = c("ISO" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(ISO, country, year, incomegroup, subsidy_posttax_IMF, subsidy_pretax_IMF) 

# USD billions (annual)

# check number of hi countries 
length(unique(fossil_subsidies$ISO)) # 74 before and 59 countries after deleting countries with just NAs

# delete countries with only NAs in subsidy_pretax
fossil_subsidies <- fossil_subsidies[complete.cases(fossil_subsidies[ , "subsidy_pretax_IMF"]),]

## save df
save(fossil_subsidies,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/fossil_subsidies.RData")

################################################## add fossil subsidies per capita & per capita subsidies change


fossil_subsidies <- fossil_subsidies %>%
  left_join(totpop, by = c("ISO" = "alpha.3", "incomegroup", "year" = "Time")) %>%
  mutate(pc.subsidy_pretax_IMF = (subsidy_pretax_IMF) / (PopTotal*1000), yearly.pc.change.subsidy_pretax_IMF=(pc.subsidy_pretax_IMF-lag(pc.subsidy_pretax_IMF))/lag(pc.subsidy_pretax_IMF)*100) %>% # multiply subsidy_pretax_IMF so that fossil_subsidies p.c. are in tCO2 & multiply PopTotal so that it is not in thousands anymore (e.g. GER 80,000 becomes 80,000,000)
  within(yearly.pc.change.subsidy_pretax_IMF[year == "2010"] <- 0) %>%
  select(country, ISO, year, incomegroup, subsidy_pretax_IMF, pc.subsidy_pretax_IMF, yearly.pc.change.subsidy_pretax_IMF)

## save df
save(fossil_subsidies,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/fossil_subsidies.RData")


################################################## 
################################################## Start plotting
################################################## 

################################################## Current level of primary energy consumption per capita

# count number of high income countries in df
length(unique(fossil_subsidies$ISO)) # 59 economies are included

# fossil_subsidies_2017 <- fossil_subsidies_2017[complete.cases(fossil_subsidies_2017[ , "pc.primary_mtoe"]),]

length(unique(fossil_subsidies$ISO)) # 59 economies remaining | Bahrain, Brunei, Curacao, New Caledonia, Taiwan dropped

results_barplot_subsidy_pretax_IMF_2017_max15USD <- fossil_subsidies %>%
  mutate(pc.subsidy_pretax_IMF = pc.subsidy_pretax_IMF * 1000000000, ToHighlight = ifelse(ISO == "GBR", "GBR", "Others")) %>%
  filter(year == 2017) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(ISO, pc.subsidy_pretax_IMF), y = pc.subsidy_pretax_IMF, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hpointline(data = eff_carb_rate_UK, aes(yintercept = pc.primary_mtoe), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  coord_cartesian(ylim = c(0, 15)) +
  labs(fill = "") +
  scale_fill_manual(values = c("GBR" = "darkred", "Others" = "gray")) + 
  # scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        text = element_text(size=9),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(# title="2017 per capita pre-tax fossil subsidies by country",
       # subtitle = "In USD (annual)",
       x= "",
       y= "Pre-tax fossil subsidies p.c. (US$ annual)")

results_barplot_subsidy_pretax_IMF_2017_max15USD

ggsave("results_barplot_subsidy_pretax_IMF_2017_max15USD.pdf")


################################################## 
################################################## Create table of high income countries with per capita fossil subsidies
################################################## 
