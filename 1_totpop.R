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

# load data
totpop <- read.csv("un2019_totpop.csv",sep=',')
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))
load("income_wb.Rdata")

##################################################

# join ISOs to totpop dataframe
totpop <- totpop %>%
  mutate(Location=tolower(Location)) %>%
  left_join(ISOs, by = c("Location" = "alternative.name")) %>%
  select(Location,alpha.3, Time, PopTotal, everything())

# join WB Income groups to totpop by alpha.3 = Code 
totpop <- totpop %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(Time > 1999, Time < 2020) %>%
  mutate(change.totpop=(PopTotal-lag(PopTotal))/lag(PopTotal)*100) %>% # build annualgrowth rate
  within(change.totpop[Time == "2000"] <- 0) %>% # set change to 0 when year = 2000 (first year of measurement)
  select(Location,alpha.3, incomegroup, Time, PopTotal, change.totpop) # add ", everything ()" to add rest of the columns

### save df
save(totpop,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/totpop.RData")

# filter for High Income countries
totpop_hi <- totpop %>%
  filter(incomegroup == "High income", Time > 1999) 

# count number of countries in df
length(unique(totpop_hi$alpha.3)) # 76 of 80 economies are included

# remove countries that have solely NAs for fossil share 
totpop_hi <- totpop_hi[complete.cases(totpop_hi[ , "PopTotal"]),]

# count number of countries in df
length(unique(totpop_hi$alpha.3)) # Still 76 of 80 economies are included

### save df
save(totpop_hi,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/totpop_hi.RData")

##################################################
load("totpop_hi.Rdata")
glimpse(totpop_hi)

# alter df to export as table to LaTex
totpop_hi <- totpop_hi %>%
  select(alpha.3, Location, Time, PopTotal) %>%
  filter(Time == 2019) %>%
  arrange(desc(PopTotal))

# safe table as LaTex file 
print(xtable(totpop_hi, caption = "Total Population Size by Country", label = "totpop2019", type = "latex"), file = "totpop_hi.tex")

##################################################

# looking at 2019 populations in 2019 across hi countries

results_plot_bar_pop_size <- totpop_hi %>%
  mutate(PopTotal = PopTotal / 1000) %>%
  filter(Time == "2019") %>% # , PopTotal > 35
  ggplot(aes(fill = PopTotal, x = reorder(alpha.3, PopTotal), y = round(PopTotal, digits = 2))) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  # scale_y_continuous(limits=c(0,300)) + 
  # scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018)) +
  # theme(panel.grid = element_blank()) +
  labs(title="Population Size by Country in 2019",
       subtitle = "",
       x= "Country",
       y="Population in millions")

results_plot_bar_pop_size # not really helpful as long as number of countries is not reduced

ggsave("results_plot_bar_pop_size.pdf")

##################################################

# looking at population trends over last 15 years in yearly change terms

results_plot_trend_y_change_pop_size <- totpop_hi %>%
  mutate(PopTotal = PopTotal / 1000) %>%
  filter(Time > 2013, Time < 2020) %>% # , PopTotal > 35
  ggplot(aes(fill = PopTotal, x = Time, y = round(change.totpop, digits = 2), group = Location)) + 
  geom_line() +
  # coord_flip() +
  # scale_y_continuous(limits=c(0,300)) + 
  # scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018)) +
  # theme(panel.grid = element_blank()) +
  labs(title="Change in population Size by Country over last 15 years",
       subtitle = "",
       x= "Country",
       y= "Yearly change in population size in %-y")

results_plot_trend_y_change_pop_size 

ggsave("results_plot_trend_y_change_pop_size.pdf")

