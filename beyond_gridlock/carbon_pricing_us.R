#
#  Impacts on US of carbon procing
#

#
# Data from EIA Annual Energy Outlook 2015
#
source('read_gcam_scenarios.R')

# BLS value for CPI-deflator to convert 2005 to 2013 dollars
# http://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=2005&year2=2013inter
deflator <- 1.19

energy_outlook <- read_excel(file.path(data_dir, 'yearbyyear.xlsx'))
energy_outlook <- energy_outlook[,-ncol(energy_outlook)]
names(energy_outlook)[1:2] <- c('key', 'label')
energy_outlook <- energy_outlook %>% filter(! is.na(key))

selection <- energy_outlook %>%
  filter(grepl('^(TCE000:ba_|CEO000:ta_Light|ESD000:xx_|ESD000:ia_|ESD000:ja_|ESD000:nom_)', key) ) %>%
  gather(key = 'year', value = 'value', -key, -label)

selection$year <- as.numeric(as.character(selection$year))
selection$value <- as.numeric(selection$value)

residential_co2_electricity <- selection %>% filter(key == 'TCE000:ba_Electricity')
residential_co2_total <- selection %>% filter(key == 'TCE000:ba_Total')
residential_electricity_consumption <- selection %>% filter(key == 'ESD000:ia_Residential')
electricity_price_2013 <- selection %>% filter(key == 'ESD000:ja_Residential')
electricity_price_nominal <- selection %>% filter(key == 'ESD000:nom_Residential')

residential_electricity_spending_2013 <- residential_electricity_consumption
residential_electricity_spending_2013$value <- residential_electricity_spending_2013$value * electricity_price_2013$value / 100.

carbon_price <- within(carbon_price, mean_price <- mean_price * deflator,
                       min_price <- min_price * deflator,
                       max_price <- max_price * deflator)

carbon_price <- carbon_price %>% select(- min_price, -max_price)
names(carbon_price)[names(carbon_price) == 'mean_price'] <- 'price'

residential_base <-   data.frame(year = residential_co2_electricity$year,
                                 emissions = residential_co2_electricity$value / 1000.,
                                 bill = residential_electricity_spending_2013$value)

residential_carbon_tax <- data.frame()
for (scenario in unique(carbon_price$Scenario)) {
  tax <- residential_base
  tax$scenario <- scenario
  price <- carbon_price %>% filter(Scenario == scenario)
  tax$price <- approx(x = price$Year, y = price$price, xout = tax$year)$y
  tax$tax <- tax$price * tax$emissions
  tax$tax.pct <- 100.0 * tax$tax / tax$bill
  residential_carbon_tax <- rbind(residential_carbon_tax, tax)
}

print(residential_carbon_tax) %>% filter(scenario == 'RCP4.5')


gasoline_tax <- function(tax) {
  # http://www.epa.gov/cleanenergy/energy-resources/refs.html
  tons_co2_per_gallon <- 8.887E-3 # tons CO2 per gallon
  tons_co2_per_gallon * tax
}

carbon_price$gas_tax <- gasoline_tax(carbon_price$price)

print(carbon_price %>% filter(Scenario == 'RCP4.5'))
