# set working directory
setwd("C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data")
dir()

install.packages("gghighlight")

# load packages
library(dplyr)
library(xlsx)
library(readxl)
library(tidyverse)
library(wbstats) # to load WB GDP data
library(WDI)
library(naniar) # to plot missing values
library(gghighlight)

# load data
emissions_terr <- read_excel("National_Carbon_emissions_2019v1.0.xlsx", sheet = "Territorial Emissions")
emissions_cons <- read_excel("National_Carbon_emissions_2019v1.0.xlsx", sheet = "Consumption Emissions")
emissions_scen <- read_excel("IPCC_Emission_reduction_benchmarks.xls", sheet = "average_yearly_reduction_rates")
load("totpop_hi.Rdata")
load("income_wb.Rdata")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))

################################################## National emissions_terr - GCB

# make df long and join isos and incomegroups (Solely "High income" countries)
emissions_terr <- emissions_terr %>%
  gather(country, co2_terr, -year) %>%
  filter(year > 1999 & year < 2020) %>%
  mutate(country=tolower(country)) %>%
  left_join(ISOs, by = c("country" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(country,alpha.3, year, incomegroup, co2_terr, everything())# set change to 0 when year = 1970 (first year of measurement)

# build emissions_terr change
emissions_terr$co2_terr <- as.numeric(emissions_terr$co2_terr)

emissions_terr <- emissions_terr %>%
  mutate(yearly.change.co2_terr=(co2_terr-lag(co2_terr))/lag(co2_terr)*100) %>% # build annualgrowth rate
  within(yearly.change.co2_terr[year == "2000"] <- 0) # set change to 0 when year = 1970 (first year of measurement) 

# add emissions per capita & per capita emissions_terr change
glimpse(emissions_terr)

totpop$Time <- as.numeric(totpop$Time)

emissions_terr <- emissions_terr %>%
  left_join(totpop, by = c("alpha.3", "incomegroup", "year" = "Time")) %>%
  mutate(pc.co2_terr = (co2_terr*3664000) / (PopTotal*1000), yearly.pc.change.co2_terr=(pc.co2_terr-lag(pc.co2_terr))/lag(pc.co2_terr)*100) %>% # multiply co2_terr so that emissions_terr p.c. are in tCO2 & multiply PopTotal so that it is not in thousands anymore (e.g. GER 80,000 becomes 80,000,000)
  within(yearly.pc.change.co2_terr[year == "2000"] <- 0) %>%
  select(country, alpha.3, year, incomegroup, co2_terr, yearly.change.co2_terr, pc.co2_terr, yearly.pc.change.co2_terr)

# save df
save(emissions_terr,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_terr.RData")

################################################## Current level of territorial emissions per capita

# count number of high income countries in df
length(unique(emissions_terr$alpha.3)) # 68 economies are included

emissions_terr <- emissions_terr[complete.cases(emissions_terr[ , "pc.co2_terr"]),]

length(unique(emissions_terr_2018$alpha.3)) # 67 economies remaining

results_barplot_co2_terr_2018 <- emissions_terr %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%
  filter(year == 2018) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(alpha.3, pc.co2_terr), y = pc.co2_terr, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hpointline(data = eff_carb_rate_UK, aes(yintercept = pc.co2_terr), color = '#4DAF4A', size = 1) +
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
  labs(# title="2018 per capita territorial carbon emissions by country",
       subtitle = "",
       x= "",
       y= "Territorial emissions p.c. in tCO2")

results_barplot_co2_terr_2018 

ggsave("results_barplot_co2_terr_2018.pdf")

################################################## ROBUSTNESS CONTROL in 8_emissions_cons

##################################################
################################################## Add Average of 2000-2005 per capita emissions 
##################################################

load("emissions_terr.Rdata")

# add average of 2000-2005 (otherwise normal pc.emissions)
emissions_terr_2000_2005 <- emissions_terr %>%
  filter(year < 2006) %>%
  group_by(alpha.3) %>%
  mutate(av.pc.co2_terr = ifelse(year < 2006, mean(pc.co2_terr), pc.co2_terr)) %>%
  select(alpha.3, year, av.pc.co2_terr)

glimpse(emissions_terr_2000_2005)

emissions_terr <- emissions_terr %>%
  left_join(emissions_terr_2000_2005, by = c("year", "alpha.3"))
  
# fill N/As with original pc_co2_terr data to allow for yearly change calculation
emissions_terr$av.pc.co2_terr <- ifelse(is.na(emissions_terr$av.pc.co2_terr), emissions_terr$pc.co2_terr, emissions_terr$av.pc.co2_terr)

# calculate yearly change from 2006-2018 vs 2000-2005 average
emissions_terr <- emissions_terr %>%
  mutate(yearly.pc.change.2000_2005_co2_terr=(av.pc.co2_terr-lag(av.pc.co2_terr))/lag(av.pc.co2_terr)*100) %>% # build annualgrowth rate
  within(yearly.pc.change.2000_2005_co2_terr[year == "2000"] <- 0)  

# save df
save(emissions_terr,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_terr.RData")

################################################## Plot yearly Trend vs Average of 2000-2005 

# prepare 2 df to allow for highlighting 

emissions_terr_grey <- emissions_terr %>%
  # group_by(alpha.3) %>%
  filter(year > 2004) 

emissions_terr_highlight <- emissions_terr %>%
  filter(year > 2004, alpha.3 == "GBR")

results_pointlineplot_av.pc.change_co2_terr <- ggplot() +
  # emissions_terr_grey
  geom_point(aes(x = year, y = yearly.change.2000_2005_co2_terr, group = alpha.3), data = emissions_terr_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = year, y = yearly.change.2000_2005_co2_terr, group = alpha.3, colour = alpha.3), data = emissions_terr_highlight) + 
  # scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid = element_blank()) +
  labs(title="Yearly change in per capita territorial emissions versus 2000-2005 average",
       subtitle = "",
       x="Years",
       y="Change in per capita territorial emissions (%yr-1)")

results_pointlineplot_av.pc.change_co2_terr

ggsave("results_pointlineplot_av.pc.change_co2_terr.pdf")

################################################## Add absolute change 

# vs average of 2000-2005 
emissions_terr <- emissions_terr %>%
  group_by(alpha.3) %>%
  mutate(average.pc.change.2000_2005_co2_terr = (av.pc.co2_terr - av.pc.co2_terr[year==2005]) / av.pc.co2_terr[year==2005] * 100)

# vs 2005
emissions_terr <- emissions_terr %>%
  group_by(alpha.3) %>%
  mutate(average.pc.change.2005_co2_terr = (pc.co2_terr - pc.co2_terr[year==2005]) / pc.co2_terr[year==2005] * 100)

# vs 2010
emissions_terr <- emissions_terr %>%
  group_by(alpha.3) %>%
  mutate(average.pc.change.2010_co2_terr = (pc.co2_terr - pc.co2_terr[year==2010]) / pc.co2_terr[year==2010] * 100)

# vs 2013 (last 5 years)
emissions_terr <- emissions_terr %>%
  group_by(alpha.3) %>%
  mutate(average.pc.change.2013_co2_terr = (pc.co2_terr - pc.co2_terr[year==2013]) / pc.co2_terr[year==2013] * 100)

# save df
save(emissions_terr,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_terr.RData")

################################################## Plot average Trend vs Average of 2000-2005, 2005, 2010, 2013  

# prepare 2 df to allow for highlighting
# change filter "year"

glimpse(emissions_terr)

emissions_terr_grey <- emissions_terr %>%
  # group_by(alpha.3) %>%
  filter(year > 2009) 

emissions_terr_highlight <- emissions_terr %>%
  mutate(ToHighlight = ifelse( alpha.3 == "GBR", "GBR", "Others")) %>%# Barplot mit durchschnittlichen jährlichen growth rates pro Land und mit den notwendigen als vhline
  filter(year > 2009, alpha.3 == "GBR")

# plot
# change year in plot name
# change year in y axis label
# change y axis variable

results_lineplot_average.pc.change.2010_co2_terr <- ggplot() +
  # emissions_terr_grey
  geom_line(aes(x = year, y = average.pc.change.2010_co2_terr, group = alpha.3), data = emissions_terr_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = year, y = average.pc.change.2010_co2_terr, group = alpha.3, colour = alpha.3), size = 1.2, data = emissions_terr_highlight) +
  scale_color_manual(values = "dark red") +
  labs(colour = "") +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .15),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(# title="Absolute change in territorial emissions p.c. versus 2000-2005 average",
       subtitle = "",
       x="Years",
       y="Change (% vs. 2010)")


results_lineplot_average.pc.change.2010_co2_terr

# vs 2000-2005 average
ggsave("results_lineplot_average.pc.change.2000_2005_co2_terr.pdf")

# vs 2005
ggsave("results_lineplot_average.pc.change.2005_co2_terr.pdf")

# vs 2010
ggsave("results_lineplot_average.pc.change.2010_co2_terr.pdf")

# vs 2013
ggsave("results_lineplot_average.pc.change.2013_co2_terr.pdf")

##################################################
################################################## Scenario data 
##################################################

glimpse(emissions_scen_plot)

# prepare df
emissions_scen <- emissions_scen %>%
  mutate(country=tolower(country)) %>%
  left_join(ISOs, by = c("country" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(country,alpha.3, year, incomegroup, everything())

# save df
save(emissions_scen,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_scen.RData")

################################################## line plot actual yearly reduction rates against neeeded reduction rates
emissions_terr_grey <- emissions_terr %>%
  # group_by(alpha.3) %>%
  filter(year > 2005) 

emissions_terr_highlight <- emissions_terr %>%
  filter(year > 2005, alpha.3 == "GBR")

emissions_scen_plot <- emissions_scen %>%
  filter(alpha.3 == "GBR", year < 2050)

class(emissions_scen_plot$year)

# rename
emissions_scen_plot$alpha.3[emissions_scen_plot$alpha.3 == "GBR"] <- "2°C Benchmark"

results_scenario_line_yearly.pc.change_co2_terr <- ggplot() +
  # emissions_terr_grey
  geom_line(aes(x = year, y = yearly.pc.change.co2_terr , group = alpha.3, colour = alpha.3), data = emissions_terr_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = year, y = yearly.pc.change.co2_terr , group = alpha.3, colour = alpha.3), size = 1.2, data = emissions_terr_highlight) + 
  geom_line(aes(x = year, y = yearly.pc.change.co2_terr , group = alpha.3, colour = alpha.3), linetype = "dashed", size = 0.8, data = emissions_scen_plot) + 
  labs(colour = "") +
  scale_color_manual(values = c("red", "dark red")) +
  scale_x_continuous(breaks = c(2006, 2010, 2015)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .2),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  # legend()
  labs(# title="Historic yearly change in p.c. terr. emissions (vs. 2000-2005 baseline) \n vs. forecasted needed yearly reduction rates in p.c. emissions",
       subtitle = "",
       x="Years",
       y="Change (%yr-1)")

results_scenario_line_yearly.pc.change_co2_terr

ggsave("results_scenario_line_yearly.pc.change_co2_terr.pdf")

################################################## 


