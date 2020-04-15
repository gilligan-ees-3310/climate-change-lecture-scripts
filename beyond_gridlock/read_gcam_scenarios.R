library(tidyverse)
library(forcats)
library(readxl)

mass_c <- 12
mass_co2 <- mass_c + 32

current_warming <- 0.61 # warming from 1850-1900 to 1986-2005 baseline

data_dir <- 'data'

rcp_45  <- read_excel(file.path(data_dir, 'GCAMRCPData.xls'),'RCP4.5')
rcp_60  <- read_excel(file.path(data_dir, 'GCAMRCPData.xls'),'GCAM6.0')
rcp_85  <- read_excel(file.path(data_dir, 'GCAMRCPData.xls'),'GCAM8.5')
rcp_26  <- read_excel(file.path(data_dir, 'GCAMRCPData.xls'),'GCAM2.6')
rcp_ref <- read_excel(file.path(data_dir, 'GCAMRCPData.xls'),'GCAMReference')
rcp_by_sector <- read_excel(file.path(data_dir, 'GCAMRCPData.xls'), 'CO2EmissionsbySector')

for(s in c('rcp_26', 'rcp_45', 'rcp_60', 'rcp_85', 'rcp_ref', 'rcp_by_sector')) {
  x <- get(s) %>% gather(key="Year", value="value", -Model, -Scenario, -Region, -Variable, -Unit)
  x$Year <- as.numeric(as.character(x$Year))
  for(c in c('Model', 'Scenario', 'Region', 'Variable')) {
    x[,c] <- factor(unlist(x[,c]))
  }
  assign(s, x)
}

all_scenarios <- rbind(rcp_26, rcp_45, rcp_60, rcp_85, rcp_ref)


fossil_only <- TRUE

cdiac <- read_excel(file.path(data_dir, 'Global_Carbon_Budget_2014_v1.1.xlsx'), 'Global Carbon Budget', skip=19)
cdiac <- cdiac[,! as.vector(is.na(cdiac[1,]))]
# names(cdiac) <- gsub(' ','.', names(cdiac), fixed=TRUE)
cdiac[,-1] <- cdiac[,-1] * mass_co2 / mass_c
cdiac$Year <- as.numeric(as.character(cdiac$Year))
cdiac <- cdiac[! is.na(cdiac$Year),]

if (fossil_only) {
  gcam_var <- 'Emissions|CO2|Fossil fuels and Industry'
  cdiac_var <- c('fossil fuel and cement emissions')
} else {
  gcam_var <- 'Emissions|CO2|Total'
  n <- names(cdiac)
  cdiac_var <- n[! n %in% c('Year', 'land sink', 'ocean sink', 'atmospheric growth')]
}


cdiac <- cdiac[,c('Year', cdiac_var)] %>%
  gather(key="Sector", value="emissions", -Year)
cdiac_total <- cdiac %>% group_by(Year) %>% summarize(emissions = sum(emissions))
cdiac_total$Scenario <- 'CDIAC'

if (fossil_only) {
  cdiac_older <- read.csv(file.path(data_dir, 'global.1751_2011.csv'), stringsAsFactors=FALSE)[-2,1:2]
  names(cdiac_older) <- c('Year','emissions')
  cdiac_older <- as.data.frame(lapply(cdiac_older, as.numeric))
  cdiac_older$emissions <- cdiac_older$emissions * (mass_co2 / mass_c) / 1000.
  cdiac_older$Scenario <- 'CDIAC'
  cdiac_older <- cdiac_older %>% filter(Year < min(cdiac_total$Year, na.rm=T))
  cdiac_total <- rbind(cdiac_older, cdiac_total)
  cdiac_total <- cdiac_total[order(cdiac_total$Year),]
}
#
# For each year and each scenario, add up global emissions and divide by 1000 to go from MT CO2 to GT CO2
#
global_emissions <- all_scenarios %>%
  filter(Variable == gcam_var) %>%
  group_by(Scenario, Year) %>%
  summarize(emissions = sum(value) / 1000) %>%
  ungroup() %>%
  mutate(Scenario = fct_expand(Scenario, 'CDIAC'))

cdiac_total <- cdiac_total %>% as_tibble() %>%
  mutate(Scenario = factor(Scenario, levels(global_emissions$Scenario)))
global_emissions <- bind_rows(global_emissions, cdiac_total[,names(global_emissions)])

global_emissions$Scenario <- ordered(global_emissions$Scenario, levels = c('GCAM2.6', 'RCP4.5', 'GCAM6.0', 'CDIAC', 'GCAMReference', 'GCAM8.5'),
                                     labels = c('RCP 2.6', 'RCP 4.5', 'RCP 6.0',  'Historical', 'Business As Usual', 'RCP 8.5'))

us_emissions <- all_scenarios %>%
  filter(Variable == gcam_var, Region == 'USA' ) %>%
  group_by(Scenario, Year) %>%
  summarize(emissions = sum(value) / 1000)

regional_cdiac <- read_excel(file.path(data_dir, 'National_Carbon_Emissions_2016_v1.0.xlsx'), 'Territorial Emissions CDIAC', skip=14,
                             col_types = "numeric", na = c('', 'NA', 'NaN', '.'))
names(regional_cdiac)[1] <- 'Year'
regional_cdiac <- regional_cdiac %>% mutate_at(vars(-Year), funs(. * mass_co2 / mass_c / 1000.))
us_cdiac <- regional_cdiac %>% select(Year, emissions = 'UNITED STATES OF AMERICA')
us_cdiac$Scenario <- 'CDIAC'

levels(us_emissions$Scenario) <- c(levels(us_emissions$Scenario), 'CDIAC')
us_emissions <- rbind(us_emissions, us_cdiac[,names(us_emissions)])

us_emissions$Scenario <- ordered(us_emissions$Scenario, levels = c('GCAM2.6', 'RCP4.5', 'GCAM6.0', 'CDIAC', 'GCAMReference', 'GCAM8.5'),
                                     labels = c('RCP 2.6', 'RCP 4.5', 'RCP 6.0',  'Historical', 'Business As Usual', 'RCP 8.5'))


# warming relative to 1986-2005 baseline
# warming <- c(RCP2.6 = 1.0, RCP4.5 = 1.8, RCP6.0 =  2.2, RCP8.5 = 3.7)
# Add baseline relative to 1850-1900
# warming <- warming + current_warming

#
# From Rogelj et al., 2012
#
# Year 2100 warming
warming.2100 <- data.frame(
  median = c(RCP2.6 = 1.5, RCP4.5 = 2.4, RCP6.0 =  3.0, RCP8.5 = 4.9),
  lower =  c(RCP2.6 = 1.3, RCP4.5 = 2.0, RCP6.0 =  2.6, RCP8.5 = 4.0),
  upper =  c(RCP2.6 = 1.9, RCP4.5 = 3.0, RCP6.0 =  3.7, RCP8.5 = 6.1)
)

# Year 2300 warming
warming.2300 <- data.frame(
  median = c(RCP2.6 = 1.1, RCP4.5 = 2.8, RCP6.0 =  4.1, RCP8.5 = 10.0),
  lower =  c(RCP2.6 = 0.9, RCP4.5 = 2.3, RCP6.0 =  3.4, RCP8.5 = 7.9),
  upper =  c(RCP2.6 = 1.5, RCP4.5 = 3.5, RCP6.0 =  5.3, RCP8.5 = 14.1)
)


warming <- warming.2100

warming_labels <- unlist(lapply(1:nrow(warming), function(i) with(warming[i,],
        substitute(paste(l, "-", u * degree *C, " warming"),
                   list(l = formatC(lower,digits=1, format='f'),
                        u = formatC(upper,digits=1, format='f'))))))
warming_labels <- c(warming_labels, 'Business as Usual', 'Historical')
names(warming_labels) <- c('RCP 2.6', 'RCP 4.5', 'RCP 6.0',  'RCP 8.5',
                           'Business As Usual', 'Historical' )

upper_year_emissions_cuts <- 2025

r85 <- as.data.frame(with(global_emissions %>% filter(Scenario == 'RCP 8.5'),
                          approx(Year, emissions, seq(2005,upper_year_emissions_cuts,1))))

r26 <- as.data.frame(with(global_emissions %>% filter(Scenario == 'RCP 2.6'),
                          approx(Year, emissions, seq(2005,upper_year_emissions_cuts,1))))

r45 <- as.data.frame(with(global_emissions %>% filter(Scenario == 'RCP 4.5'),
                          approx(Year, emissions, seq(2005,upper_year_emissions_cuts,1))))

r60 <- as.data.frame(with(global_emissions %>% filter(Scenario == 'RCP 6.0'),
                          approx(Year, emissions, seq(2005,upper_year_emissions_cuts,1))))

rref <- as.data.frame(with(global_emissions %>% filter(Scenario == 'Business As Usual'),
                           approx(Year, emissions, seq(2005,upper_year_emissions_cuts,1))))

calc_cuts <- function(scenario) {
  scenario_name <- paste0('RCP ',formatC(scenario/10.0, digits=1, format='f'))
  delta <- eval(as.name(paste0('r', scenario)))
  delta$y85 <- (r85$y - delta$y)
  delta$yref <- (rref$y - delta$y)

  cat('Getting from RCP 8.5 to ', scenario_name, ' between 2016 and ',
      upper_year_emissions_cuts, ':\n\tCumulative cut = ', round(sum(delta$y85),1),
      ' billion tons,\n\t',
      'or ', round(sum(delta$y85) / 10.,1),
      ' billion tons per year over the next 10 years.\n\t',
      'The annual cut would be ',
      round(100 * sum(delta$y85) / (10 * tail(cdiac$emissions,1)),1),
      "% of today's total global emissions.\n", sep='')
  cat('Getting from Buinsess As Usual to ', scenario_name, ' between 2016 and ',
      upper_year_emissions_cuts, ':\n\tCumulative cut = ', round(sum(delta$yref),1),
      ' billion tons,\n\t',
      'or ', round(sum(delta$yref) / 10.,1),
      ' billion tons per year over the next 10 years.\n\t',
      'The annual cut would be ',
      round(100 * sum(delta$yref) / (10 * tail(cdiac$emissions,1)),1),
      "% of today's total global emissions.\n", sep='')
}

calc_cuts(26)
calc_cuts(45)
calc_cuts(60)

eia.2014 <- data.frame(Year      = c( 1990, 2012, 2020, 2030, 2040),
                       newpolicy = c(20938,31615,34203,36291,38037)/1000,
                       bau       = c(20938,31615,35523,40848,45950)/1000,
                       s450      = c(20938,31615,32479,25424,19300)/1000
)

newlevels <- c(levels(global_emissions$Scenario), 's450', 'newpolicy', 'bau')
newdata <- as.data.frame(global_emissions)
newdata$Scenario <- ordered(newdata$Scenario, levels = newlevels)
eia.2014 <- eia.2014 %>% gather(key = Scenario, value = emissions, -Year)

eia.2014$Scenario <- ordered(eia.2014$Scenario, levels = newlevels)

eia.2014 <- eia.2014[,names(newdata)]
newdata <- rbind(newdata, eia.2014)
newdata$Scenario <- ordered(newdata$Scenario, levels = newlevels)

carbon_price <- all_scenarios %>%
  filter(Variable == 'Price|Carbon') %>%
  group_by(Scenario, Year) %>%
  summarize(mean_price = mean(value, na.rm=T), max_price = max(value, na.rm=T),
            min_price = min(value, na.rm=T)) %>%
  ungroup()

print(carbon_price %>%
        select(Year, Scenario, mean_price) %>%
        spread(key = Scenario, value = mean_price))
