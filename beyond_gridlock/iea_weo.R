#
#  IEA Scenarios
#
scenario_new_policies <- data.frame(
  Year = c(1990, 2012, 2020, 2025, 2030, 2035, 2040),
  co2 = c(20938, 31615, 34203, 35370, 36291, 37163, 38037),
  Scenario = 'IEA New Policies'
)
scenario_current_policies <- data.frame(
  Year = c(2012, 2020, 2030, 2040),
  co2 = c(31615, 35523, 40848, 45950),
  Scenario = 'IEA Current Policies'
)

scenario_450 <- data.frame(
  Year = c(2012, 2020, 2030, 2040),
  co2 = c(31615, 32479, 25424, 19300),
  Scenario = 'IEA 450'
)

iea_weo <- rbind(scenario_new_policies, scenario_current_policies, scenario_450)
names(iea_weo) <- c('Year', 'emissions', 'Scenario')
iea_weo <- iea_weo[,c('Scenario', 'Year', 'emissions')]
iea_weo$emissions <- iea_weo$emissions / 1000

cdiac_fossil <- cdiac %>% group_by(Year) %>% filter(Sector == 'fossil fuel and cement emissions')
names(cdiac_fossil)[names(cdiac_fossil)=='Emissions'] <- 'emissions'
cdiac_fossil$Scenario <- 'CDIAC'


global_fossil_emissions <- all_scenarios %>% filter(Variable == 'Emissions|CO2|Fossil fuels and Industry')
global_fossil_emissions <- global_fossil_emissions %>% group_by(Scenario, Year) %>% summarize(emissions = sum(value)/1000)
levels(global_fossil_emissions$Scenario) <- c(levels(global_fossil_emissions$Scenario), 'CDIAC')
global_fossil_emissions <- rbind(global_fossil_emissions, cdiac_fossil[,names(global_fossil_emissions)])
global_fossil_emissions$Scenario <- ordered(global_fossil_emissions$Scenario, 
                                            levels = c('GCAM2.6', 'RCP4.5', 'GCAM6.0', 'CDIAC', 'GCAMReference', 'GCAM8.5'),
                                            labels = c('RCP 2.6', 'RCP 4.5', 'RCP 6.0',  'Historical', 'Business As Usual', 'RCP 8.5'))

global_fossil_emissions <- rbind(global_fossil_emissions %>% filter(! Scenario %in% unique(iea_weo$Scenario) ), iea_weo)

p <- ggplot(global_fossil_emissions %>% filter(Scenario != 'Historical'), aes(x=Year, y=emissions, color=Scenario, group=Scenario)) +
  geom_hline(yintercept = 0, color='black') +
  geom_line(size=1) + 
  geom_point(size=3) +
  geom_line(size=1, data = global_fossil_emissions %>% filter(Scenario == 'Historical' & Year >= 1990)) +
  scale_x_continuous(breaks=seq(1990,2100,15)) +
  labs(x = "Year", y = expression(paste(CO[2], " emissions (billions of tons)"))) +
  theme_bw()

print(p)
