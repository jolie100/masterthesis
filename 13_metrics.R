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

# load data
metrics <- read_excel("metrics_evaluation.xlsx")

# safe table as LaTex file 
print(xtable(metrics, caption = "National GHG targets of high income countries of the EU", label = "metrics", type = "latex"), file = "metrics.tex")

