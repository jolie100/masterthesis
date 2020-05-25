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
energy <- read_excel("bp-stats-review-2019-consolidated.xlsx")
load("income_wb.Rdata")
load("totpop.Rdata")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))
energy_scen <- read_excel("IPCC_Emission_reduction_benchmarks.xls", sheet = "pe_av_yearly_reduction_rates")

################################################## 
################################################## Create df
################################################## 

################################################## create energy wide with mtoe as unit
energy_wide <- energy %>% 
  spread(Var, Value) %>%
  filter(Year > 1999 & Year < 2020) %>%
  select(Country,
         Year,
         ISO3166_alpha3,
         primary_mtoe,
         nuclear_mtoe,
         oilcons_mtoe,
         hydro_mtoe,
         renewables_mtoe,
         coalcons_mtoe,
         geothermal_mtoe,
         gascons_mtoe,
         biomass_mtoe,
         solar_mtoe,
         wind_mtoe,
         biogeo_mtoe
  ) %>%
  left_join(income_wb, by = c("ISO3166_alpha3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(Country,
         ISO3166_alpha3,
         Year,
         incomegroup,
         primary_mtoe,
         everything())

length(unique(energy_wide$ISO3166_alpha3)) # 49 countries included

energy_wide <- energy_wide[complete.cases(energy_wide[ , "primary_mtoe"]),]

length(unique(energy_wide$ISO3166_alpha3)) # 45 economies remaining | Bahrain, Brunei, Curacao, New Caledonia dropped

# save energy_wide
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")

# save energy
save(energy,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy.RData")

################################################## add primary per capita & per capita primary change
glimpse(totpop)

totpop$Time <- as.numeric(totpop$Time)

energy_wide <- energy_wide %>%
  left_join(totpop, by = c("ISO3166_alpha3" = "alpha.3", "incomegroup", "Year" = "Time")) %>%
  mutate(pc.primary_mtoe = (primary_mtoe) / (PopTotal*1000), Yearly.pc.change.primary_mtoe=(pc.primary_mtoe-lag(pc.primary_mtoe))/lag(pc.primary_mtoe)*100) %>% # multiply primary_mtoe so that energy_wide p.c. are in tCO2 & multiply PopTotal so that it is not in thousands anymore (e.g. GER 80,000 becomes 80,000,000)
  within(Yearly.pc.change.primary_mtoe[Year == "2000"] <- 0) %>%
  select(Country, ISO3166_alpha3, Year, incomegroup, primary_mtoe, pc.primary_mtoe, Yearly.pc.change.primary_mtoe, everything())

energy_wide <- energy_wide[complete.cases(energy_wide[ , "pc.primary_mtoe"]),]

length(unique(energy_wide$ISO3166_alpha3)) # 44 economies remaining | Taiwan dropped (no population data)

# save df
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")

################################################## Add Average of 2000-2005 per capita emissions 

# add average of 2000-2005 (otherwise normal pc.emissions)
energy_wide_2000_2005 <- energy_wide %>%
  filter(Year < 2006) %>%
  group_by(ISO3166_alpha3) %>%
  mutate(av.pc.primary_mtoe = ifelse(Year < 2006, mean(pc.primary_mtoe), pc.primary_mtoe)) %>%
  select(ISO3166_alpha3, Year, av.pc.primary_mtoe, everything())

glimpse(energy_wide)

energy_wide <- energy_wide %>%
  left_join(energy_wide_2000_2005, by = c("Year", "ISO3166_alpha3"))

# fill N/As with original pc_primary_mtoe data to allow for Yearly change calculation
energy_wide$av.pc.primary_mtoe <- ifelse(is.na(energy_wide$av.pc.primary_mtoe), energy_wide$pc.primary_mtoe, energy_wide$av.pc.primary_mtoe)

################################################## calculate Yearly change from 2006-2018 vs 2000-2005 average
energy_wide <- energy_wide %>%
  mutate(Yearly.pc.change.2000_2005_primary_mtoe=(av.pc.primary_mtoe-lag(av.pc.primary_mtoe))/lag(av.pc.primary_mtoe)*100) %>% # build annualgrowth rate
  within(Yearly.pc.change.2000_2005_primary_mtoe[Year == "2000"] <- 0)  

# save df
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")


################################################## add absolute change

glimpse(energy_wide)

# versus 2000-2005 baseline
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(average.pc.change.2000_2005_primary_mtoe = (av.pc.primary_mtoe - av.pc.primary_mtoe[Year==2005]) / av.pc.primary_mtoe[Year==2005] * 100)

# vs 2005
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(pc.change.2005_primary_mtoe = (pc.primary_mtoe - pc.primary_mtoe[Year==2005]) / pc.primary_mtoe[Year==2005] * 100)

# vs 2010
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(pc.change.2010_primary_mtoe = (pc.primary_mtoe - pc.primary_mtoe[Year==2010]) / pc.primary_mtoe[Year==2010] * 100)

# vs 2013 (last 5 years)
energy_wide <- energy_wide %>%
  group_by(ISO3166_alpha3) %>%
  mutate(pc.change.2013_primary_mtoe = (pc.primary_mtoe - pc.primary_mtoe[Year==2013]) / pc.primary_mtoe[Year==2013] * 100)


# save df
save(energy_wide,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/energy_wide.RData")



################################################## 
################################################## Start plotting
################################################## 

################################################## Current level of primary energy consumption per capita

load("energy_wide.Rdata")

# count number of high income countries in df
length(unique(energy_wide$ISO3166_alpha3)) # 49 economies are included

# remove countries with na in 2018
energy_wide_2018 <- energy_wide %>%
  filter(Year == 2018) #%>%
  # transform mtoe in toe for plot
  #mutate(pc.primary_toe = pc.primary_mtoe * 1000000)

# energy_wide_2018 <- energy_wide_2018[complete.cases(energy_wide_2018[ , "pc.primary_mtoe"]),]

# length(unique(energy_wide_2018$ISO3166_alpha3)) # 44 economies remaining | Bahrain, Brunei, Curacao, New Caledonia, Taiwan dropped

results_barplot_primary_toe_2018 <- energy_wide_2018 %>%
  mutate(pc.primary_toe = pc.primary_mtoe * 1000000, ToHighlight = ifelse(ISO3166_alpha3 == "GBR", "GBR", "Others")) %>%
  # filter(Year == 2018) %>% # , PopTotal > 35
  ggplot(aes(x = reorder(ISO3166_alpha3, pc.primary_toe), y = pc.primary_toe, fill = ToHighlight)) + # order ascending
  geom_bar(stat = "identity") +
  # geom_hpointline(data = eff_carb_rate_UK, aes(yintercept = pc.primary_mtoe), color = '#4DAF4A', size = 1) +
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
  labs(# title="2018 per capita primary energy consumption by country",
       # subtitle = "In tonnes of oil equivalent",
       x= "",
       y= "Primary energy consumption p.c. (toe)")

results_barplot_primary_toe_2018 

ggsave("results_barplot_primary_toe_2018.pdf")


################################################## Plot absolute Trend 

# prepare 2 df to allow for highlighting 
# change year in filter

glimpse(energy_wide)

energy_wide_grey <- energy_wide %>%
  # group_by(ISO3166_alpha3) %>%
  filter(Year > 2012) 

energy_wide_highlight <- energy_wide %>% # Barplot mit durchschnittlichen jährlichen growth rates pro Land und mit den notwendigen als vhline
  filter(Year > 2012, ISO3166_alpha3 == "GBR")

# plot
# change year in plot name and y axis title
# change y axis variable

results_lineplot_2013.pc.change_primary_mtoe <- ggplot() +
  # energy_wide_grey
  geom_line(aes(x = Year, y = pc.change.2013_primary_mtoe, group = ISO3166_alpha3), data = energy_wide_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = Year, y = pc.change.2013_primary_mtoe, group = ISO3166_alpha3, colour = ISO3166_alpha3), size = 1.2, data = energy_wide_highlight) + 
  scale_color_manual(values = "dark red") +
  labs(colour = "") +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .15),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  labs(# title="Average change in per capita primary energy consumption vs. 2000-2005 average",
       # subtitle = "",
       x="Years",
       y="Change (% vs. 2013)")


results_lineplot_2013.pc.change_primary_mtoe

# vs 2000 2005 average
ggsave("results_lineplot_average20002005.pc.change_primary_mtoe.pdf")

# vs 2005
ggsave("results_lineplot_2005.pc.change_primary_mtoe.pdf")

# vs 2010
ggsave("results_lineplot_2010.pc.change_primary_mtoe.pdf")

# vs 2013
ggsave("results_lineplot_2013.pc.change_primary_mtoe.pdf")

################################################## line plot actual Yearly reduction rates against neeeded reduction rates

# prepare df
glimpse(energy_wide)

energy_scen <- energy_scen %>%
  mutate(country=tolower(country)) %>%
  left_join(ISOs, by = c("country" = "alternative.name")) %>%
  left_join(income_wb, by = c("alpha.3" = "Code")) %>%
  filter(incomegroup == "High income") %>%
  select(country,alpha.3, year, incomegroup, everything())

# plot
energy_wide_grey <- energy_wide %>%
  # group_by(ISO3166_alpha3) %>%
  filter(Year > 2005) 

energy_wide_highlight <- energy_wide %>%
  filter(Year > 2005, ISO3166_alpha3 == "GBR")

energy_scen_plot <- energy_scen %>%
  filter(alpha.3 == "GBR", year < 2050)

class(energy_scen_plot$Year)

# rename
energy_scen_plot$alpha.3[energy_scen_plot$alpha.3 == "GBR"] <- "2°C Benchmark"

results_scenario_line_Yearly.pc.change_primary_mtoe <- ggplot() +
  # energy_wide_grey
  geom_line(aes(x = Year, y = Yearly.pc.change.primary_mtoe, group = ISO3166_alpha3, colour = ISO3166_alpha3), data = energy_wide_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = Year, y = Yearly.pc.change.primary_mtoe, group = ISO3166_alpha3, colour = ISO3166_alpha3), size = 1.2, data = energy_wide_highlight) + 
  geom_line(aes(x = year, y = Yearly.pc.change.primary_mtoe, group = alpha.3, colour = alpha.3), linetype = "dashed", size = 0.8, data = energy_scen_plot) + 
  scale_x_continuous(breaks = c(2006, 2010, 2015)) +
  labs(colour = "") +
  scale_color_manual(values = c("red", "dark red")) +
  theme(panel.grid = element_blank(),
        legend.position = c(.01, .2),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  # legend()
  labs(# title="Historic Yearly change in p.c. primary energy consumption (vs. 2000-2005 baseline) \n vs. forecasted needed yearly reduction rates in p.c. consumption",
       # subtitle = "",
       x="Years",
       y="Change (%yr-1)")


results_scenario_line_Yearly.pc.change_primary_mtoe

ggsave("results_scenario_line_Yearly.pc.change_primary_mtoe.pdf")
