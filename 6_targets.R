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
targets_nationalEU <- read_excel("NDC_Database_CAIT_World_Resource_Institute_06042020.xlsx", sheet = "EU_national_adapted")
ndcs_international <- read_excel("CW_NDC_Content_26062018.xlsx", sheet = "CAIT")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))
load("income_wb.Rdata")


################################################## NDC_international data CAIT World Resource Institute
glimpse(ndcs_international)

# prepare df
ndcs_international <- ndcs_international %>%
  left_join(income_wb, by = c("ISO" = "Code")) %>% # join income groups
  select(Country, ISO, incomegroup, everything()) 

# safe df
save(ndcs_international,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/ndcs_international.Rdata")

################################################## AMBITION - NDC_international data CAIT World Resource Institute
# create df ambition (without sectoral coverage)
ndcs_international_ambition <- ndcs_international %>%
  filter(grepl("GHG target", mitigation_contribution_type), incomegroup == "High income", !grepl("Non-GHG target and", mitigation_contribution_type)) %>% # filtering for GHG targets leaves 24 High Income countries excluding the EU
  # filter(incomegroup == "High income") %>% # `Mitigation contribution type` == "GHG target", 
  select(Country, ISO, ghg_target, ghg_target_type, reference_base_year, time_target_year, conditionality) 

# reference_base_emissions, reference_base_emissions_intensity, reference_projected_baseline_emissions, time_target_year, conditionality

# 52 countries remain

# safe table as LaTex file 
print(xtable(ndcs_international_ambition, caption = "GHG targets of high income countries under the Paris Agreement", label = "ndcs_international_ambition", type = "latex"), file = "ndcs_international_ambition.tex")

# safe df
save(ndcs_international_ambition,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/ndcs_international_ambition.Rdata")

################################################## COVERAGE - NDC_international data CAIT World Resource Institute
# create df coverage (without sectoral coverage)
ndcs_international_coverage <- ndcs_international %>%
  filter(grepl("GHG target", mitigation_contribution_type), incomegroup == "High income", !grepl("Non-GHG target and", mitigation_contribution_type)) %>% # filtering for GHG targets leaves 24 High Income countries excluding the EU
  # filter(incomegroup == "High income") %>% # `Mitigation contribution type` == "GHG target", 
  select(Country, ISO, coverage_sectors_label, coverage_sectors_short, coverage_sectors) 

# safe table as LaTex file 
print(xtable(ndcs_international_coverage, caption = "GHG targets of high income countries under the Paris Agreement", label = "ndcs_international_coverage", type = "latex"), file = "ndcs_international_coverage.tex")

# safe df
save(ndcs_international_coverage,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/ndcs_international_coverage.Rdata")

################################################## NDC_international data CAIT World Resource Institute (Only economy wide)
glimpse(targets_nationalEU)

# prepare df
targets_nationalEU <- targets_nationalEU %>%
  mutate(Country=tolower(Country)) %>%
  left_join(ISOs, by = c("Country" = "alternative.name")) %>%
  select(Country,alpha.3, everything())

targets_nationalEU <- targets_nationalEU %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>% # join income groups
  select(Country, alpha.3, `Law/Policy`, Year, ghg_target, ghg_target_type, reference_base_year, time_target_year, `Target level of emissions`) 

# safe table as LaTex file 
print(xtable(targets_nationalEU, caption = "National GHG targets of high income countries of the EU", label = "targets_nationalEU", type = "latex"), file = "targets_nationalEU.tex")

# safe df
save(targets_nationalEU,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/targets_nationalEU.Rdata")
