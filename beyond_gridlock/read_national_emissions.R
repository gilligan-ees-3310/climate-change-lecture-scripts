library(tidyverse)
library(stringr)

lines <- read_lines('data/nation.1751_2014.csv')
lines <- lines[-(2:4)]
lines[1] <- lines[1] %>% str_split(',') %>% unlist() %>%
  str_replace_all('"', '') %>%
  str_replace_all(c('Total CO2 emissions from .*$' = 'total',
                              '^Emissions from +' = '',
                              ' +fuel' = '',
                              ' +(consumption|production)$' = '',
                              'Per capita CO2 emissions.*$' = 'per-capita',
                              '^bunker.+$' = 'bunker')) %>%
  str_to_lower() %>% str_replace_all('[^a-z0-9_]+', '.') %>%
  str_c(collapse = ',')
lines <- str_c(lines, collapse = '\n')
national_cdiac <- read_csv(lines, col_types =  'cidddddddd',
                           na = c('', '.', 'NA'), trim_ws = TRUE)
