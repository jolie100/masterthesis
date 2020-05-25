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
load("energy_wide.Rdata")
load("income_wb.Rdata")
# income_wb <- read_excel("wb2019income.xls", sheet = "income")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))
fossil_scen <- read_excel("IPCC_Emission_reduction_benchmarks.xls", sheet = "fs_av_yearly_reduction_rates")

load("emissions_terr.Rdata")


################################################## Fossil share with energy_wide as base
load("energy_wide.Rdata")

glimpse(energy_wide)

# add yearly.change.fossil.share | unit juggling for potential carbon intensity calculation
energy_wide <- energy_wide %>%
  mutate(primary_kg = fossil.share * 1000000000,
         fossil_energy_kg = (coalcons_mtoe + oilcons_mtoe + gascons_mtoe) * 1000000000,
         fossil.share = fossil_energy_kg / primary_kg, # use above built variable fossil energy
         yearly.change.fossil.share = (fossil.share-lag(fossil.share))/lag(fossil.share)*100) %>%
  within(yearly.change.fossil.share[Year == "2000"] <- 0)
  
 
################################################## Add Average of 2000-2005 fossil share 

# add average of 2000-2005 (otherwise normal emissions)
energy_wide_2000_2005 <- energy_wide %>%
  filter(Year < 2006) %>%
  group_by(ISO3166_alpha3) %>%
  mutate(av.fossil.share = mean(fossil.share)) %>%
  select(ISO3166_alpha3, Year, fossil.share, av.fossil.share)

glimpse(energy_wide)

energy_wide <- energy_wide %>%
  left_join(energy_wide_2000_2005, by = c("Year", "ISO3166_alpha3", "fossil.share"))

# fill N/As with original pc_fossil.share data to allow for Yearly change calculation
energy_wide$av.fossil.share <- ifelse(is.na(energy_wide$av.fossil.share), energy_wide$fossil.share, energy_wide$av.fossil.share)

# count number of countries in df
length(unique(energy_wide$ISO3166_alpha3)) # 44 of 80 economies are included

# save df
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/fossil.share.RData")

################################################## calculate Yearly change from 2006-2018 vs 2000-2005 average
energy_wide <- energy_wide %>%
  mutate(Yearly.change.2000_2005_fossil.share =(av.fossil.share-lag(av.fossil.share))/lag(av.fossil.share)*100) %>% # build annualgrowth rate
  within(Yearly.change.2000_2005_fossil.share[Year == "2000"] <- 0)  

# save df
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")


################################################## add absolute change
glimpse(energy_wide)

# versus 2000-2005 baseline
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(average.change.2000_2005_fossil.share = (av.fossil.share - av.fossil.share[Year==2005]) / av.fossil.share[Year==2005] * 100)

# vs 2005
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(average.change.2005_fossil.share = (fossil.share - fossil.share[Year==2005]) / fossil.share[Year==2005] * 100)

# vs 2010
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(average.change.2010_fossil.share = (fossil.share - fossil.share[Year==2010]) / fossil.share[Year==2010] * 100)

# vs 2013 (last 5 years)
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(average.change.2013_fossil.share = (fossil.share - fossil.share[Year==2013]) / fossil.share[Year==2013] * 100)

# save df
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")


################################################## 
################################################## Start plotting
################################################## 

################################################## Current level of fossil share

# count number of high income countries in df
length(unique(energy_wide$ISO3166_alpha3)) # 44 economies are included

# remove countries with na in 2018
energy_wide_2018 <- energy_wide %>%
  filter(Year == 2018) #%>%
  select(Year, ISO3166_alpha3, average.pc.change.2000_2005_primary_mtoe)

# energy_wide_2018 <- energy_wide_2018[complete.cases(energy_wide_2018[ , "fossil.share"]),]

length(unique(energy_wide$ISO3166_alpha3)) # 44 economies remaining | Bahrain, Brunei, Curacao, New Caledonia, Taiwan dropped

results_barplot_fossil.share_2018 <- energy_wide %>%
  mutate(ToHighlight = ifelse(ISO3166_alpha3 == "GBR", "GBR", "Others"), fossil.share = fossil.share*100) %>%
  filter(Year == 2018) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(ISO3166_alpha3, fossil.share), y = fossil.share, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hpointline(data = eff_carb_rate_UK, aes(yintercept = fossil.share), color = '#4DAF4A', size = 1) +
  # coord_flip() +
  labs(fill = "") +
  scale_fill_manual(values = c("GBR" = "darkred", "Others" = "gray")) + 
  #scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(breaks = 2015) +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1), 
        text = element_text(size=9),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(#title="2018 Share of primary energy consumption fueled by fossil sources by country",
       #subtitle = "",
       x= "",
       y= "Share (%)")

results_barplot_fossil.share_2018 

ggsave("results_barplot_fossil.share_2018.pdf")


################################################## Plot average Trend vs Average of 2000-2005 

# prepare 2 df to allow for highlighting 
# change year in filter

glimpse(energy_wide)

energy_wide_grey <- energy_wide %>%
  # group_by(ISO3166_alpha3) %>%
  filter(Year > 2004) #%>%
  #mutate(average.pc.change.2000_2005_fossil.share = average.pc.change.2000_2005_fossil.share/100)

energy_wide_highlight <- energy_wide %>% 
  filter(Year > 2004, ISO3166_alpha3 == "GBR") #%>%
  #mutate(average.pc.change.2000_2005_fossil.share = average.pc.change.2000_2005_fossil.share/100)

# plot
# change year in plot name and y axis title
# change y axis variable

results_lineplot_average.change_20002005_fossil.share <- ggplot() +
  # energy_wide_grey
  geom_line(aes(x = Year, y = average.pc.change.2000_2005_fossil.share, group = ISO3166_alpha3), data = energy_wide_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = Year, y = average.pc.change.2000_2005_fossil.share, group = ISO3166_alpha3, colour = ISO3166_alpha3), size = 1.2, data = energy_wide_highlight) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "dark red") +
  labs(colour = "") +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .15),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(#title="Average change fossil share in primary energy consumption vs. 2000-2005 average",
       #subtitle = "",
       x="Years",
       y="Change (% vs. 2005)")



results_lineplot_average.change_20002005_fossil.share

# vs 2000 2005
ggsave("results_lineplot_average.change_20002005_fossil.share.pdf")

# vs 2005
ggsave("results_lineplot_average.change_2005_fossil.share.pdf")

# vs 2010
ggsave("results_lineplot_average.change_2010_fossil.share.pdf")

# vs 2013
ggsave("results_lineplot_average.change_2013_fossil.share.pdf")



################################################## line plot actual Yearly reduction rates against neeeded reduction rates

# prepare df
glimpse(fossil_scen)

fossil_scen <- fossil_scen %>%
  mutate(country=tolower(country)) %>%
  left_join(ISOs, by = c("country" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(country,alpha.3, year, incomegroup, everything())

# plot
energy_wide_grey <- energy_wide %>%
  # group_by(ISO3166_alpha3) %>%
  filter(Year > 2005) #%>%
  #mutate(Yearly.pc.change.2000_2005_fossil.share = Yearly.pc.change.2000_2005_fossil.share/100)

energy_wide_highlight <- energy_wide %>%
  filter(Year > 2005, ISO3166_alpha3 == "GBR")#%>%
  #mutate(Yearly.pc.change.2000_2005_fossil.share = Yearly.pc.change.2000_2005_fossil.share/100)

fossil_scen_plot <- fossil_scen %>%
  filter(alpha.3 == "GBR", year < 2050)#%>%
  #mutate(Yearly.change.fossil.share = Yearly.change.fossil.share/100)

# rename
fossil_scen_plot$alpha.3[fossil_scen_plot$alpha.3 == "GBR"] <- "2°C Benchmark"

results_scenario_line_Yearly.change_fossil.share <- ggplot() +
  # energy_wide_grey
  geom_line(aes(x = Year, y = yearly.change.fossil.share, group = ISO3166_alpha3, colour = ISO3166_alpha3), data = energy_wide_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = Year, y = yearly.change.fossil.share, group = ISO3166_alpha3, colour = ISO3166_alpha3), size = 1.2, data = energy_wide_highlight) + 
  geom_line(aes(x = year, y = Yearly.change.fossil.share, group = alpha.3, colour = alpha.3), linetype = "dashed", size = 0.8, data = fossil_scen_plot) + 
  coord_cartesian(ylim = c(-20, 20)) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2006, 2010, 2015, 2020)) +
  #scale_y_continuous(labels = scales::percent) +
  labs(colour = "") +
  scale_color_manual(values = c("red", "dark red")) +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .2),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  # legend()
  labs(# title="Historic yearly change fossil share (vs. 2000-2005 baseline) \n vs. forecasted needed yearly reduction rates in consumption",
       # subtitle = "",
       x="Years",
       y="Change (%yr-1)")

results_scenario_line_Yearly.change_fossil.share

ggsave("results_scenario_line_Yearly.change_fossil.share.pdf")




