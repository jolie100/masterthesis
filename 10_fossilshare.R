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
energy <- read_excel("bp-stats-review-2019-consolidated.xlsx")
load("income_wb.Rdata")
# income_wb <- read_excel("wb2019income.xls", sheet = "income")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))

################################################## create energy wide with mtoe as unit
energy_wide <- energy %>% 
  spread(Var, Value) %>%
  filter(Year > 1999 & Year < 2020) %>%
  select(Country,
         Year,
         ISO3166_alpha3,
         primary_mtoe,
         nuclear_mtoe,
         oilcons_mtoe,
         hydro_mtoe,
         renewables_mtoe,
         coalcons_mtoe,
         geothermal_mtoe,
         gascons_mtoe,
         biomass_mtoe,
         solar_mtoe,
         wind_mtoe,
         biogeo_mtoe
  )

str(energy_wide)

# save energy_wide
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")

# save energy
save(energy,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy.RData")

################################################## Income groups

# count number of countries in df

income_wb_hi <- income_wb %>%
  filter(incomegroup == "High income")

length(unique(income_wb_hi$Code)) # 80 countries included

################################################## Fossil share with energy_wide as base
load("energy_wide.Rdata")
load("fossil_share_final.Rdata")

fossil_share_final <- energy_wide %>%
  mutate(primary_kg = primary_mtoe * 1000000000,
         fossil_energy_kg = (coalcons_mtoe + oilcons_mtoe + gascons_mtoe) * 1000000000,
         fossil.share = fossil_energy_kg / primary_kg, # use above built variable fossil energy
         change.fossil.share = (fossil.share-lag(fossil.share))/lag(fossil.share)*100)

# join WB Income groups to fossil share final by alpha.3 = Code df
fossil_share_final <- fossil_share_final %>% 
  left_join(income_wb, by = c("ISO3166_alpha3" = "Code")) %>%
  select(Country, ISO3166_alpha3, incomegroup, Year, fossil.share, change.fossil.share, everything())

# filter for High Income countries
fossil_share_final_hi <- fossil_share_final %>%
  filter(incomegroup == "High income", Year > 1999) 

# count number of countries in df
length(unique(fossil_share_final_hi$ISO3166_alpha3)) # 49 of 80 economies are included

# remove countries that have solely NAs for fossil share 
fossil_share_final_hi <- fossil_share_final_hi[complete.cases(fossil_share_final_hi[ , "fossil.share"]),]

# count number of countries in df
length(unique(fossil_share_final_hi$ISO3166_alpha3)) # 45 of 80 economies are included

# save fossil_share
save(fossil_share_final,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/fossil_share_final.RData")






