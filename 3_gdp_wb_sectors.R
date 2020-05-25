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

# looking at shares of sectors in GDP to see whether some countries are more production intensive than others
# NV.AGR.TOTL.ZS - 1-5 ISIC - Agriculture
# NV.IND.TOTL.ZS - 10-45 ISIC - Industry
# NV.SRV.TOTL.ZS - 50-99 ISIC - Services

# load data 
gdp_wb_sectors <- wb(indicator = c("NV.AGR.TOTL.ZS", "NV.IND.TOTL.ZS", "NV.SRV.TOTL.ZS"), mrv = 49, cache = new_wb_cache) # ,  "NY.GDP.PCAP.PP.KD" (p.c.)
load("income_wb.Rdata")

# spread df
gdp_wb_sectors <- gdp_wb_sectors %>%
  select(-indicatorID) %>%
  spread(indicator, value)

# join WB Income groups to gdp_wb_sectors by isoc = Code 
gdp_wb_sectors <- gdp_wb_sectors %>%
  left_join(income_wb, by = c("iso3c" = "Code")) %>%
  select(iso3c, country, incomegroup, date, everything())

### save df
save(gdp_wb_sectors,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/gdp_wb_sectors.Rdata")

##################################################

# filter for High Income countries
gdp_wb_sectors_hi <- gdp_wb_sectors %>%
  filter(incomegroup == "High income") 

# count number of countries in df
length(unique(gdp_wb_sectors_hi$iso3c)) # 55 of 80 economies are included

# remove countries that have solely NAs  
gdp_wb_sectors_hi <- gdp_wb_sectors_hi[complete.cases(gdp_wb_sectors_hi[ , "Agriculture, forestry, and fishing, value added (% of GDP)"]),]
gdp_wb_sectors_hi <- gdp_wb_sectors_hi[complete.cases(gdp_wb_sectors_hi[ , "Industry (including construction), value added (% of GDP)"]),]
gdp_wb_sectors_hi <- gdp_wb_sectors_hi[complete.cases(gdp_wb_sectors_hi[ , "Services, value added (% of GDP)"]),]

# count number of countries in df
length(unique(gdp_wb_sectors_hi$iso3c)) # 53 economies are included

# save df
save(gdp_wb_sectors_hi,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/gdp_wb_sectors_hi.Rdata")

##################################################

load("gdp_wb_sectors_hi.Rdata")

glimpse(gdp_wb_sectors_hi)

# alter df to export as table to LaTex
gdp_wb_sectors_hi <- gdp_wb_sectors_hi %>%
  filter(date == 2018) %>%
  select(-iso2c) %>%
  select(iso3c, country, date, everything())

# safe table as LaTex file 
print(xtable(gdp_wb_sectors_hi, caption = "Sectoral Share of GDP by Country in 2018", label = "gdppc", type = "latex"), file = "gdp_wb_sectors_hi.tex")
