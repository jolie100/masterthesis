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
emissions_cons <- read_excel("National_Carbon_emissions_2019v1.0.xlsx", sheet = "Consumption Emissions")
emissions_scen <- read_excel("IPCC_Emission_reduction_benchmarks.xls", sheet = "average_yearly_reduction_rates")
load("totpop_hi.Rdata")
load("income_wb.Rdata")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))

################################################## National emissions_cons - GCB
# make df long and join isos and incomegroups (Solely "High income" countries)
emissions_cons <- emissions_cons %>%
  gather(country, co2_cons, -year) %>%
  filter(year > 1999 & year < 2020) %>%
  mutate(country=tolower(country)) %>%
  left_join(ISOs, by = c("country" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(country,alpha.3, year, incomegroup, co2_cons, everything())# set change to 0 when year = 1970 (first year of measurement)

# build emissions_cons change
emissions_cons$co2_cons <- as.numeric(emissions_cons$co2_cons)

emissions_cons <- emissions_cons %>%
  mutate(change.co2_cons=(co2_cons-lag(co2_cons))/lag(co2_cons)*100) %>% # build annualgrowth rate
  within(change.co2_cons[year == "2000"] <- 0) # set change to 0 when year = 1970 (first year of measurement) 

# add emissions per capita & per capita emissions_cons change
glimpse(emissions_cons)

emissions_cons <- emissions_cons %>%
  left_join(totpop, by = c("alpha.3", "incomegroup", "year" = "Time")) %>%
  mutate(pc.co2_cons = (co2_cons*3664000) / (PopTotal*1000), pc.change.co2_cons=(pc.co2_cons-lag(pc.co2_cons))/lag(pc.co2_cons)*100) %>% # multiply co2_cons so that emissions_cons p.c. are in tCO2 & multiply PopTotal so that it is not in thousands anymore (e.g. GER 80,000 becomes 80,000,000)
  within(pc.change.co2_cons[year == "2000"] <- 0) %>%
  select(country, alpha.3, year, incomegroup, co2_cons, change.co2_cons, pc.co2_cons, pc.change.co2_cons)

# save df
save(emissions_cons,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_cons.RData")


################################################## Current level of consumption based emissions per capita
load("emissions_cons.Rdata")

# count number of high income countries in df
length(unique(emissions_cons$alpha.3)) # 68 economies are included

emissions_cons <- emissions_cons[complete.cases(emissions_cons[ , "pc.co2_cons"]),]

length(unique(emissions_cons$alpha.3)) # 48 economies remaining

results_barplot_co2_cons_2017 <- emissions_cons %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%
  filter(year == 2017) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(alpha.3, pc.co2_cons), y = pc.co2_cons, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hline(data = eff_carb_rate_UK, aes(yintercept = pc.co2_cons), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  labs(fill = "") +
  scale_fill_manual(values = c("GBR" = "darkred", "Others" = "gray")) + 
  # scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        text = element_text(size=9),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(# title="2017 per capita consumption based carbon emissions by country",
       subtitle = "",
       x= "",
       y= "Consumption based emissions p.c. in tCO2")

results_barplot_co2_cons_2017 

ggsave("results_barplot_co2_cons_2017.pdf")

################################################## CONTROL alter terr df for comparison to consumption based emissions

emissions_terr_control_2017 <- emissions_terr

glimpse(emissions_terr_control_2017)

# join emissions terr 2017 to emissions con to have same countries in bar plot
emissions_terr_control_2017 <- emissions_cons %>%
  left_join(emissions_terr, by = c("country", "alpha.3", "year", "incomegroup"))

################################################## Plot 2017 level of territorial emissions per capita with 48 countries

# count number of high income countries in df
length(unique(emissions_terr_control_2017$alpha.3)) # 48 economies are included

emissions_terr_control_2017 <- emissions_terr_control_2017[complete.cases(emissions_terr_control_2017[ , "pc.co2_terr"]),]

length(unique(emissions_terr_control_2017$alpha.3)) # 48 economies remaining

results_barplot_co2_terr_2017control <- emissions_terr_control_2017 %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%
  filter(year == 2017) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(alpha.3, pc.co2_terr), y = pc.co2_terr, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hline(data = eff_carb_rate_UK, aes(yintercept = pc.co2_terr), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  labs(fill = "") +
  scale_fill_manual(values = c("GBR" = "darkred", "Others" = "gray")) + 
  # scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        text = element_text(size=9),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(# title="2017 per capita territorial carbon emissions by country",
       subtitle = "",
       x= "",
       y= "Territorial emissions p.c. in tCO2")

results_barplot_co2_terr_2017control 

ggsave("results_barplot_co2_terr_2017control.pdf")

################################################## Plot average Trend vs 2005 with consumption based emissions

glimpse(emissions_cons)

# add abolut change
# vs 2005
emissions_cons <- emissions_cons %>%
  group_by(alpha.3) %>%
  mutate(average.pc.change.2005_co2_cons = (pc.co2_cons - pc.co2_cons[year==2005]) / pc.co2_cons[year==2005] * 100)

# vs 2010
emissions_cons <- emissions_cons %>%
  group_by(alpha.3) %>%
  mutate(average.pc.change.2010_co2_cons = (pc.co2_cons - pc.co2_cons[year==2010]) / pc.co2_cons[year==2010] * 100)

# prepare 2 df to allow for highlighting 

emissions_cons_grey <- emissions_cons %>%
  # group_by(alpha.3) %>%
  filter(year > 2009) # , alpha.3 != "PAN"

emissions_cons_highlight <- emissions_cons %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%# Barplot mit durchschnittlichen jährlichen growth rates pro Land und mit den notwendigen als vhline
  filter(year > 2009, alpha.3 == "GBR")

results_lineplot_average.pc.change.2010_co2_cons <- ggplot() +
  # emissions_terr_grey
  geom_line(aes(x = year, y = average.pc.change.2010_co2_cons, group = alpha.3), data = emissions_cons_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = year, y = average.pc.change.2010_co2_cons, group = alpha.3, colour = alpha.3), size = 1.2, data = emissions_cons_highlight) +
  scale_color_manual(values = "dark red") +
  labs(colour = "") +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .15),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(# title="Absolute change in consumption based emissions p.c. versus 2005",
    subtitle = "",
    x="Years",
    y="Change (% vs. 2010)")


results_lineplot_average.pc.change.2010_co2_cons

# vs 2005
ggsave("results_lineplot_average.pc.change.2005_co2_cons.pdf")

# vs 2010
ggsave("results_lineplot_average.pc.change.2010_co2_cons.pdf")
