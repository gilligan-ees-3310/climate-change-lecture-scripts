library(tidyverse)
library(stringr)
library(readxl)

if (! exists('data_dir')) {
  source('semester_config.R')
}

load_farm_data <- function(filename) {
  x <- read_excel(filename, '10304 Ann', skip=7)
  names(x)[2:3] <- c('item', 'code')
  names(x) <- names(x) %>% str_trim() %>% str_to_lower() %>% str_replace_all(" +", ".")
  x <- x %>% filter(! is.na(line))
  x <- x %>% select(-line, -code) %>% 
    mutate(item = str_to_lower(str_replace_all(str_trim(item), ' +', '.'))) %>% 
    filter(item %in% c('farm', 'gross.domestic.product')) %>% 
    gather(key = "year", value = "gdp", -item, convert=TRUE) %>% 
    spread(key = "item", value = "gdp") %>% 
    mutate(farm.prices = farm / gross.domestic.product)
  invisible(x)
}

get_farm_prices <- function(min.year = NA, max.year = NA) {
  recent_prices <- load_farm_data(file.path(data_dir, 'agriculture', 'Section1All_xls.xls'))
  historical_prices <- load_farm_data(file.path(data_dir, 'agriculture', 'Section1All_Hist.xls'))
  prices <- rbind(historical_prices, recent_prices) %>% arrange(year)
  if (! is.na(min.year)) prices <- prices %>% filter(year >= min.year)
  if (! is.na(max.year)) prices <- prices %<% filter(year <= max.year)
  prices <- prices %>% mutate(pct = 100 * farm.prices / max(farm.prices))
  invisible(prices)
}
