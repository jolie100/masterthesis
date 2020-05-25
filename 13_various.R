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
library(xtable)

# load data metris table
metrics <- read_excel("metrics_evaluation.xlsx")

# safe table as LaTex file 
print(xtable(metrics, caption = "National GHG targets of high income countries of the EU", label = "metrics", type = "latex"), file = "metrics.tex")

##############################################################################

# load data sectors table
sectors <- read_excel("ipcc_ar6_edgar_data_countries_sectors.xlsx", sheet = "sector_classification_altered")

# safe table as LaTex file 
print(xtable(sectors, caption = "Sector definitions by EDGAR", label = "sectordefinitions", type = "latex"), file = "sectors.tex")



