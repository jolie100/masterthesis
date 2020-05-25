# set working directory
setwd("C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data")
dir()

install.packages("gghighlight")
install.packages("gridExtra")

# load packages
library(dplyr)
library(xlsx)
library(readxl)
library(tidyverse)
library(wbstats) # to load WB GDP data
library(WDI)
library(naniar) # to plot missing values
library(gghighlight)
library(gridExtra)

load("totpop_hi.Rdata")
load("income_wb.Rdata")
ISOs <- read_excel("data_ISOcodes.xlsx",sheet = "alternative_names") %>%
  mutate(alternative.name=tolower(alternative.name))
emissions_sect <- read_excel("ipcc_ar6_edgar_data_countries_sectors.xlsx", sheet = "emissions_data")

################################################## prepare emissions_sect - EDGAR
glimpse(emissions_sect)

emissions_sect$CO2_MtC <- as.numeric(emissions_sect$CO2_MtC)

# create new column with million tons of carbon (same unit as in 'emissions')
emissions_sect <- emissions_sect %>% 
rename(sector = chapter_title) %>%
  mutate(CO2_MtC = CO2 / 3664000) %>%
  filter(year > 1999 & year < 2020) %>%
  select(ISO, country, year, sector, CO2_MtC) %>%
  spread(sector, CO2_MtC) %>%
  rename(Energy = `Energy systems`)

##################################################  add average of 2000-2005 (otherwise normal pc.emissions)
emissions_sect_2000_2005 <- emissions_sect %>%
  filter(year < 2006) %>%
  group_by(ISO) %>%
  mutate(AFOLU.av.co2 = ifelse(year < 2006, mean(AFOLU), AFOLU)) %>%
  mutate(Buildings.av.co2 = ifelse(year < 2006, mean(Buildings), Buildings)) %>%
  mutate(Energy.av.co2 = ifelse(year < 2006, mean(Energy), Energy)) %>%
  mutate(Industry.av.co2 = ifelse(year < 2006, mean(Industry), Industry)) %>%
  mutate(Transport.av.co2 = ifelse(year < 2006, mean(Transport), Transport)) %>%
  select(ISO, year, everything())

glimpse(emissions_sect)

emissions_sect <- emissions_sect %>%
  left_join(emissions_sect_2000_2005, by = c("year", "ISO", "country", "AFOLU", "Buildings", "Energy", "Industry", "Transport"))

# fill N/As with original sectoral data to allow for yearly change calculation
emissions_sect$AFOLU.av.co2 <- ifelse(is.na(emissions_sect$AFOLU.av.co2), emissions_sect$AFOLU, emissions_sect$AFOLU.av.co2)
emissions_sect$Buildings.av.co2 <- ifelse(is.na(emissions_sect$Buildings.av.co2), emissions_sect$Buildings, emissions_sect$Buildings.av.co2)
emissions_sect$Energy.av.co2 <- ifelse(is.na(emissions_sect$Energy.av.co2), emissions_sect$Energy, emissions_sect$Energy.av.co2)
emissions_sect$Industry.av.co2 <- ifelse(is.na(emissions_sect$Industry.av.co2), emissions_sect$Industry, emissions_sect$Industry.av.co2)
emissions_sect$Transport.av.co2 <- ifelse(is.na(emissions_sect$Transport.av.co2), emissions_sect$Transport, emissions_sect$Transport.av.co2)

##################################################  join income groups and filter for high income
emissions_sect <- emissions_sect %>%
  left_join(income_wb, by = c("ISO" = "Code")) %>%
  filter(incomegroup == "High income")

# remove countries that only contain missings for one of the sectors (They have then NAs for all others as well)
emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "AFOLU"]),]
emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Industry"]),]
emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Energy"]),]
emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Buildings"]),]
emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Transport"]),]

length(unique(emissions_sect$ISO)) # 67 economies remaining

################################################## add average change vs 2000-2005 base period
emissions_sect <- emissions_sect %>%
  group_by(ISO) %>%
  mutate(AFOLU.av.co2.change = (AFOLU.av.co2 - AFOLU.av.co2[year==2005]) / AFOLU.av.co2[year==2005] * 100,
         Buildings.av.co2.change = (Buildings.av.co2 - Buildings.av.co2[year==2005]) / Buildings.av.co2[year==2005] * 100,
         Energy.av.co2.change = (Energy.av.co2 - Energy.av.co2[year==2005]) / Energy.av.co2[year==2005] * 100,
         Industry.av.co2.change = (Industry.av.co2 - Industry.av.co2[year==2005]) / Industry.av.co2[year==2005] * 100,
         Transport.av.co2.change = (Transport.av.co2 - Transport.av.co2[year==2005]) / Transport.av.co2[year==2005] * 100)

# save df
save(emissions_sect,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_sect.RData")

################################################## add absolute change 2005 base year
emissions_sect <- emissions_sect %>%
  group_by(ISO) %>%
  mutate(AFOLU.co2.change = (AFOLU - AFOLU[year==2005]) / AFOLU[year==2005] * 100,
         Buildings.co2.change = (Buildings - Buildings[year==2005]) / Buildings[year==2005] * 100,
         Energy.co2.change = (Energy - Energy[year==2005]) / Energy[year==2005] * 100,
         Industry.co2.change = (Industry - Industry[year==2005]) / Industry[year==2005] * 100,
         Transport.co2.change = (Transport - Transport[year==2005]) / Transport[year==2005] * 100)

################################################## add absolute change 2010 base year
emissions_sect <- emissions_sect %>%
  group_by(ISO) %>%
  mutate(AFOLU.co2.change.2010 = (AFOLU - AFOLU[year==2010]) / AFOLU[year==2010] * 100,
         Buildings.co2.change.2010 = (Buildings - Buildings[year==2010]) / Buildings[year==2010] * 100,
         Energy.co2.change.2010 = (Energy - Energy[year==2010]) / Energy[year==2010] * 100,
         Industry.co2.change.2010 = (Industry - Industry[year==2010]) / Industry[year==2010] * 100,
         Transport.co2.change.2010 = (Transport - Transport[year==2010]) / Transport[year==2010] * 100)

# save df
save(emissions_sect,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_sect.RData")



################################################## 
################################################## Plot 
################################################## 

load("emissions_sect.Rdata")
  
glimpse(emissions_sect)

# make df long with 2000-2005
emissions_sect_20002005_change_long <- emissions_sect %>%
  select(ISO, country, year, AFOLU.av.co2.change, Buildings.av.co2.change, Transport.av.co2.change, Industry.av.co2.change, Energy.av.co2.change) %>%
  gather("Sector", "co2_terr_sect", -ISO, -country, -year)

################################################## Prepare plot against 2005

# make df long with 2005
emissions_sect_2005_change_long <- emissions_sect %>%
  select(ISO, country, year, AFOLU.co2.change, Buildings.co2.change, Transport.co2.change, Industry.co2.change, Energy.co2.change) %>%
  rename(AFOLU = AFOLU.co2.change, Buildings = Buildings.co2.change, Transport = Transport.co2.change, Industry = Industry.co2.change, Energy = Energy.co2.change) %>%
  gather("Sector", "co2_terr_sect", -ISO, -country, -year)

glimpse(emissions_sect_2005_change_long)

emissions_sect_grey <- emissions_sect_2005_change_long %>%
  # group_by(alpha.3) %>%
  filter(year > 2004) 

emissions_sect_highlight <- emissions_sect_2005_change_long %>%
  filter(year > 2004, ISO == "GBR")

################################################## Prepare plot against 2010

# make df long with 2010
emissions_sect_2010_change_long <- emissions_sect %>%
  select(ISO, country, year, AFOLU.co2.change.2010, Buildings.co2.change.2010, Transport.co2.change.2010, Industry.co2.change.2010, Energy.co2.change.2010) %>%
  rename(AFOLU = AFOLU.co2.change.2010, Buildings = Buildings.co2.change.2010, Transport = Transport.co2.change.2010, Industry = Industry.co2.change.2010, Energy = Energy.co2.change.2010) %>%
  gather("Sector", "co2_terr_sect", -ISO, -country, -year)

glimpse(emissions_sect_2010_change_long)


emissions_sect_grey <- emissions_sect_2010_change_long %>%
  # group_by(alpha.3) %>%
  filter(year > 2009) 

emissions_sect_highlight <- emissions_sect_2010_change_long %>%
  filter(year > 2009, ISO == "GBR")

################################################## Plot
# change
## year in plot name
## year y axis label

results_lineplot_emissions_sect_2010_change <- ggplot() +
  # emissions_terr_grey
  geom_line(aes(x = year, y = co2_terr_sect , group = ISO, colour = ISO), data = emissions_sect_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = year, y = co2_terr_sect , group = ISO, colour = ISO), size = 0.8, data = emissions_sect_highlight) + 
  facet_wrap(~Sector, scales = "free") +
  # geom_line(aes(x = year, y = yearly.pc.change.co2_terr , group = alpha.3, colour = alpha.3), linetype = "dashed", size = 0.8, data = emissions_scen_plot) + 
  labs(colour = "") +
  coord_cartesian(ylim = c(-100, 300)) +
  scale_color_manual(values = "dark red") +
  scale_x_continuous(breaks = c(2005, 2010, 2015)) +
  # scale_y_continuous(limits = c(-150, 500)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.001, .15),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  # legend()
  labs(# title="Historic yearly change in p.c. terr. emissions (vs. 2000-2005 baseline) \n vs. forecasted needed yearly reduction rates in p.c. emissions",
    subtitle = "",
    x="Years",
    y="Change (% vs. 2010)")


results_lineplot_emissions_sect_2010_change 

ggsave("results_lineplot_emissions_sect_2010_change.pdf")











##################################################
##################################################  join totpop and build pc sectoral emissions
##################################################

glimpse(emissions_sect)

emissions_sect <- emissions_sect %>%
  left_join(totpop_hi, by = c("ISO" = "alpha.3", "year" = "Time")) %>%
  mutate(AFOLU.pc = (AFOLU) / (PopTotal*1000),
         Buildings.pc = (Buildings) / (PopTotal*1000),
         Energy.pc = (Energy) / (PopTotal*1000),
         Industry.pc = (Industry) / (PopTotal*1000),
         Transport.pc = (Transport) / (PopTotal*1000))
        
# remove countries that only contain missings for one of the sectors (They have then NAs for all others as well)
         emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "AFOLU.pc"]),]
         emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Buildings.pc"]),]
         emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Energy.pc"]),]
         emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Industry.pc"]),]
         emissions_sect <- emissions_sect[complete.cases(emissions_sect[ , "Transport.pc"]),]
         
length(unique(emissions_sect$ISO)) # 66 economies remaining

################################################## add absolute change 2005 base year
emissions_sect <- emissions_sect %>%
  group_by(ISO) %>%
  mutate(AFOLU.co2.pc.change = (AFOLU.pc - AFOLU.pc[year==2005]) / AFOLU.pc[year==2005] * 100,
         Buildings.co2.pc.change = (Buildings.pc - Buildings.pc[year==2005]) / Buildings.pc[year==2005] * 100,
         Energy.co2.pc.change = (Energy.pc - Energy.pc[year==2005]) / Energy.pc[year==2005] * 100,
         Industry.co2.pc.change = (Industry.pc - Industry.pc[year==2005]) / Industry.pc[year==2005] * 100,
         Transport.co2.pc.change = (Transport.pc - Transport.pc[year==2005]) / Transport.pc[year==2005] * 100)

################################################## add absolute change 2010 base year
emissions_sect <- emissions_sect %>%
  group_by(ISO) %>%
  mutate(AFOLU.pc.co2.change.2010 = (AFOLU.pc - AFOLU.pc[year==2010]) / AFOLU.pc[year==2010] * 100,
         Buildings.pc.co2.change.2010 = (Buildings.pc - Buildings.pc[year==2010]) / Buildings.pc[year==2010] * 100,
         Energy.pc.co2.change.2010 = (Energy.pc - Energy.pc[year==2010]) / Energy.pc[year==2010] * 100,
         Industry.pc.co2.change.2010 = (Industry.pc - Industry.pc[year==2010]) / Industry.pc[year==2010] * 100,
         Transport.pc.co2.change.2010 = (Transport.pc - Transport.pc[year==2010]) / Transport.pc[year==2010] * 100)

# save df
save(emissions_sect,file="C:/Users/User/iCloudDrive/Universität/Master - HSoG/Master Thesis/Daten/data/emissions_sect.RData")

################################################## Prepare plot against 2005

# make df long with 2005
emissions_sect_pc_2005_change_long <- emissions_sect %>%
  select(ISO, country, year, AFOLU.co2.pc.change, Buildings.co2.pc.change, Transport.co2.pc.change, Industry.co2.pc.change, Energy.co2.pc.change) %>%
  rename(AFOLU = AFOLU.co2.pc.change, Buildings = Buildings.co2.pc.change, Transport = Transport.co2.pc.change, Industry = Industry.co2.pc.change, Energy = Energy.co2.pc.change) %>%
  gather("Sector", "co2_terr_sect", -ISO, -country, -year)

glimpse(emissions_sect_pc_2005_change_long)

emissions_sect_grey <- emissions_sect_pc_2005_change_long %>%
  # group_by(alpha.3) %>%
  filter(year > 2004) 

emissions_sect_highlight <- emissions_sect_pc_2005_change_long %>%
  filter(year > 2004, ISO == "GBR")

################################################## Prepare plot against 2010

# make df long with 2010
emissions_sect_2010_change_long <- emissions_sect %>%
  select(ISO, country, year, AFOLU.co2.change.2010, Buildings.co2.change.2010, Transport.co2.change.2010, Industry.co2.change.2010, Energy.co2.change.2010) %>%
  rename(AFOLU = AFOLU.co2.change.2010, Buildings = Buildings.co2.change.2010, Transport = Transport.co2.change.2010, Industry = Industry.co2.change.2010, Energy = Energy.co2.change.2010) %>%
  gather("Sector", "co2_terr_sect", -ISO, -country, -year)

glimpse(emissions_sect_2010_change_long)


emissions_sect_grey <- emissions_sect_2010_change_long %>%
  # group_by(alpha.3) %>%
  filter(year > 2009) 

emissions_sect_highlight <- emissions_sect_2010_change_long %>%
  filter(year > 2009, ISO == "GBR")

################################################## Plot
# change
## year and "pc" in plot name
## year y axis label

results_lineplot__pc_emissions_sect_2005_change <- ggplot() +
  # emissions_terr_grey
  geom_line(aes(x = year, y = co2_terr_sect , group = ISO, colour = ISO), data = emissions_sect_grey, colour = alpha("grey", 0.7)) +
  # colourise GBR data
  geom_line(aes(x = year, y = co2_terr_sect , group = ISO, colour = ISO), size = 0.8, data = emissions_sect_highlight) + 
  facet_wrap(~Sector, scales = "free") +
  # geom_line(aes(x = year, y = yearly.pc.change.co2_terr , group = alpha.3, colour = alpha.3), linetype = "dashed", size = 0.8, data = emissions_scen_plot) + 
  labs(colour = "") +
  coord_cartesian(ylim = c(-100, 300)) +
  scale_color_manual(values = "dark red") +
  scale_x_continuous(breaks = c(2005, 2010, 2015)) +
  # scale_y_continuous(limits = c(-150, 500)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.001, .15),
        legend.justification = c("left", "top"),
        legend.background = element_blank()) +
  # legend()
  labs(# title="Historic yearly change in p.c. terr. emissions (vs. 2000-2005 baseline) \n vs. forecasted needed yearly reduction rates in p.c. emissions",
    subtitle = "",
    x="Years",
    y="Change (% vs. 2005)")


results_lineplot__pc_emissions_sect_2005_change 

ggsave("results_lineplot__pc_emissions_sect_2005_change.pdf")


