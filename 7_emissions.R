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
emissions_terr <- read_excel("National_Carbon_emissions_2019v1.0.xlsx", sheet = "Territorial Emissions")
emissions_cons <- read_excel("National_Carbon_emissions_2019v1.0.xlsx", sheet = "Consumption Emissions")
emissions_scen <- read_excel("IPCC_Emission_reduction_benchmarks.xls", sheet = "average_yearly_reduction_rates")
load("totpop_hi.Rdata")
load("income_wb.Rdata")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))

################################################## National emissions_terr - GCB
# make df long and join isos and incomegroups (Solely "High income" countries)
emissions_terr <- emissions_terr %>%
  gather(country, co2_terr, -year) %>%
  filter(year > 1999 & year < 2020) %>%
  mutate(country=tolower(country)) %>%
  left_join(ISOs, by = c("country" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(country,alpha.3, year, incomegroup, co2_terr, everything())# set change to 0 when year = 1970 (first year of measurement)

# build emissions_terr change
emissions_terr$co2_terr <- as.numeric(emissions_terr$co2_terr)

emissions_terr <- emissions_terr %>%
  mutate(change.co2_terr=(co2_terr-lag(co2_terr))/lag(co2_terr)*100) %>% # build annualgrowth rate
  within(change.co2_terr[year == "2000"] <- 0) # set change to 0 when year = 1970 (first year of measurement) 

# add emissions per capita & per capita emissions_terr change
glimpse(emissions_terr)

emissions_terr <- emissions_terr %>%
  left_join(totpop, by = c("alpha.3", "incomegroup", "year" = "Time")) %>%
  mutate(pc.co2_terr = (co2_terr*3664000) / (PopTotal*1000), pc.change.co2_terr=(pc.co2_terr-lag(pc.co2_terr))/lag(pc.co2_terr)*100) %>% # multiply co2_terr so that emissions_terr p.c. are in tCO2 & multiply PopTotal so that it is not in thousands anymore (e.g. GER 80,000 becomes 80,000,000)
  within(pc.change.co2_terr[year == "2000"] <- 0) %>%
  select(country, alpha.3, year, incomegroup, co2_terr, change.co2_terr, pc.co2_terr, pc.change.co2_terr)

# save df
save(emissions_terr,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_terr.RData")

################################################## Current level of territorial emissions per capita

# count number of high income countries in df
length(unique(emissions_terr$alpha.3)) # 68 economies are included

# remove countries with na in 2018
emissions_terr_2018 <- emissions_terr %>%
  filter(year == 2018)

emissions_terr_2018 <- emissions_terr_2018[complete.cases(emissions_terr_2018[ , "pc.co2_terr"]),]

length(unique(emissions_terr_2018$alpha.3)) # 67 economies remaining

results_barplot_co2_terr <- emissions_terr_2018 %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%
  # filter(year == 2018) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(alpha.3, pc.co2_terr), y = pc.co2_terr, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hline(data = eff_carb_rate_UK, aes(yintercept = pc.co2_terr), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  scale_fill_manual(values = c("GBR" = "tomato", "Others" = "gray")) + 
  # scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=9)) +
  labs(title="2018 per capita territorial carbon emissions by country",
       subtitle = "",
       x= "",
       y= "Per capita territorial carbon emissions in tCO2")

results_barplot_co2_terr 

ggsave("results_barplot_co2_terr.pdf")

################################################## Current level of consumption based emissions per capita

# count number of high income countries in df
length(unique(emissions_cons$alpha.3)) # 68 economies are included

# remove countries with na in 2018
emissions_terr_2018 <- emissions_terr %>%
  filter(year == 2018)

emissions_terr_2018 <- emissions_terr_2018[complete.cases(emissions_terr_2018[ , "pc.co2_terr"]),]

length(unique(emissions_terr_2018$alpha.3)) # 67 economies remaining

results_barplot_co2_terr <- emissions_terr_2018 %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%
  # filter(year == 2018) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(alpha.3, pc.co2_terr), y = pc.co2_terr, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hline(data = eff_carb_rate_UK, aes(yintercept = pc.co2_terr), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  scale_fill_manual(values = c("GBR" = "tomato", "Others" = "gray")) + 
  # scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=9)) +
  labs(title="2018 per capita territorial carbon emissions by country",
       subtitle = "",
       x= "",
       y= "Per capita territorial carbon emissions in tCO2")

results_barplot_co2_terr 

ggsave("results_barplot_co2_terr.pdf")
