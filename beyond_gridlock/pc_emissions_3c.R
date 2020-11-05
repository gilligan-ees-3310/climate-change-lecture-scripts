library(tidyverse)
library(stringr)

# Developed nations
all_scenarios %>%
  filter(Region %in% c('Australia_NA', 'Canada', 'USA', 'Western Europe', 'Japan', 'Korea') &
           Scenario == 'RCP4.5' &
           Variable %in% c('Population|Total', 'Emissions|CO2|Total')) %>%
  group_by(Year, Variable) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(Variable = str_replace_all(Variable, '^([A-Za-z]+)\\|.*$', '\\1')) %>%
  spread(key = Variable, value = value) %>%
  mutate(pc.emissions = Emissions / Population, ratio = pc.emissions / pc.emissions[1])

# Whole world
all_scenarios %>%
  filter(Scenario == 'RCP4.5' & Variable %in% c('Population|Total', 'Emissions|CO2|Total')) %>%
  group_by(Year, Variable) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(Variable = str_replace(Variable, "^([a-zA-Z]+)\\|.*$", "\\1")) %>%
  spread(key = Variable, value = value) %>%
  mutate(PC.Emissions = Emissions / Population, ratio = PC.Emissions / PC.Emissions[1])

