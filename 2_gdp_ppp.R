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
# load income_group data
income_wb <- read_excel("wb2019income.xls", sheet = "income")
# check available data
str(wb_cachelist, max.level = 1)
# download data
new_wb_cache <- wbcache()
# load gdp data in PPP
gdp_wb <- wb(indicator = c("NY.GDP.MKTP.PP.KD"), mrv = 49, cache = new_wb_cache) # ,  "NY.GDP.PCAP.PP.KD" (p.c.)
load("totpop.Rdata")

##################################################

glimpse(gdp_wb)

gdp_wb <- gdp_wb %>%
  filter(date > 1999) %>%
  select(iso3c, country, date, value, indicator) %>% 
  rename(unit = indicator, gdp = value) %>%
  arrange(iso3c, date) %>%
  mutate(change.gdp = (gdp-lag(gdp))/lag(gdp)*100) %>%
  within(change.gdp[date == "2000"] <- 0) # set change to 0 when year = 2000 (first year of measurement)

# join WB Income groups to gdp_wb by isoc = Code 
gdp_wb <- gdp_wb %>%
  left_join(income_wb, by = c("iso3c" = "Code")) %>%
  select(iso3c, country, incomegroup, date, everything())

# build GDP per capita
gdp_wb$date <- as.numeric(gdp_wb$date)# make date numeric 

gdp_pc_wb <- left_join(gdp_wb, totpop, by = c("iso3c" = "alpha.3", "date" = "Time", "incomegroup")) %>%
  mutate(gdp.pc = gdp / (PopTotal*1000), change.gdp.pc = (gdp.pc-lag(gdp.pc))/lag(gdp.pc)*100) %>%
  within(change.gdp.pc[date == "2000"] <- 0)

# add gdp pcto gdp_wb
gdp_wb <- gdp_pc_wb %>% 
  select(iso3c, Location, incomegroup, date, gdp, unit, change.gdp, gdp.pc, change.gdp.pc)

### save df
save(gdp_wb,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/gdp_wb.Rdata")

##################################################

# filter for High Income countries
gdp_wb_hi <- gdp_wb %>%
  filter(incomegroup == "High income") 

# count number of countries in df
length(unique(gdp_wb_hi$iso3c)) # 64 of 80 economies are included

# remove countries that have solely NAs for fossil share 
gdp_wb_hi <- gdp_wb_hi[complete.cases(gdp_wb_hi[ , "gdp.pc"]),]

# count number of countries in df
length(unique(gdp_wb_hi$iso3c)) # 63 economies are included

# save fossil_share
save(gdp_wb_hi,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/gdp_wb_hi.Rdata")

##################################################
load("gdp_wb_hi.Rdata")

glimpse(gdp_wb_hi)

# alter df to export as table to LaTex
gdp_wb_hi <- gdp_wb_hi %>%
  filter(date == 2018) %>%
  select(iso3c, Location, date, gdp.pc) %>%
  arrange(desc(gdp.pc))

# safe table as LaTex file 
print(xtable(gdp_wb_hi, caption = "GDP per capita in PPP constant 2011 international $ by Country", label = "gdppc", type = "latex"), file = "gdp_pc_wb_hi_ppp.tex")

##################################################

# looking at gdp per capita trends over last 15 years in yearly change terms

results_plot_trend_y_change_gdp_pc <- gdp_wb_hi %>%
  filter(date > 2002, date < 2020) %>% # , PopTotal > 35
  ggplot(aes(x = date, y = round(change.gdp.pc, digits = 2), group = iso3c)) + 
  geom_line() +
  # coord_flip() +
  # scale_y_continuous(limits=c(0,300)) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018)) +
  # theme(panel.grid = element_blank()) +
  labs(title="Change in GDP per capita by Country over last 15 years",
       subtitle = "",
       x= "Country",
       y= "Yearly change in GDP per capita in %-y")

results_plot_trend_y_change_gdp_pc 

ggsave("results_plot_trend_y_change_gdp_pc.pdf")

