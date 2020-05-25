# set working directory
setwd("C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data")
dir()

# install.packages("WDI")

# load packages
library(dplyr)
library(xlsx)
library(readxl)
library(tidyverse)
library(wbstats) # to load WB GDP data
library(WDI)
library(naniar) # to plot missing values

# load data 
countries_carbonprice <- read_excel("WB2019_Carbon_Pricing__Data_Dashboard.xls.xlsx", sheet = "countries")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))
load("income_wb.Rdata")

# join ISOs to countries_carbonprice dataframe and filter for hi countries
countries_carbonprice <- countries_carbonprice %>%
  mutate(`Jurisdiction covered`=tolower(`Jurisdiction covered`)) %>%
  left_join(ISOs, by = c("Jurisdiction covered" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(alpha.3, `Year of implementation`, `Name of the initiative`, Type) %>%
  #select(-Status, -`World Bank region` ) %>%
  arrange(alpha.3)

# save df
save(countries_carbonprice,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/countries_carbonprice.Rdata")

################################################## Countries with carbon pricing schemes: 36

load("countries_carbonprice.Rdata")

glimpse(countries_carbonprice)

# safe table as LaTex file 
print(xtable(countries_carbonprice, caption = "Countries with implemented carbon prices", label = "cntrs_carbpnprice", type = "latex"), file = "countries_carbonprice.tex")

# count number of countries in df
length(unique(countries_carbonprice$alpha.3)) # 36 economies are included

################################################## Effective carbon rates for 33 of those 36 countries in 5_effcarbrate_oecd




