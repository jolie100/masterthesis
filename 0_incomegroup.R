# set working directory
setwd("C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data")
dir()

# install.packages("stargazer")

# load packages
library(dplyr)
library(xlsx)
library(readxl)
library(tidyverse)
library(wbstats) # to load WB GDP data
library(WDI)
library(naniar) # to plot missing values
library(stargazer)# to make tables of descriptive statsitics
library(xtable)

glimpse(income_wb)

# load data
income_wb <- read_excel("wb2019income.xls", sheet = "income")

# count number of countries in df
income_wb_hi <- income_wb %>%
  filter(incomegroup == "High income")

length(unique(income_wb_hi$Code)) # 80 economies

# rename income group
names(income_wb)[names(income_wb) == "Income group"] <- "incomegroup"

# narrow down df to iso3 and incomegroup
income_wb <- income_wb %>% select(Code, incomegroup)

save(income_wb,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/income_wb.RData")
