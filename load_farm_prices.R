library(tidyverse)
library(readxl)
library(janitor)

# Files downloaded from US Bureau of Economic Analysis.
# National Income and Product Accounts (NIPA)
# https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=2

if (! exists('data_dir')) {
  source('semester_config.R')
}

load_farm_data <- function(filename) {
  x <- read_excel(filename, 'T10304-A', skip=7, .name_repair = "minimal")
  names(x)[2:3] <- c('item', 'code')
  names(x) <- names(x) %>% make_clean_names()
  x <- x %>% filter(! is.na(line), ! is.na(code))
  x <- x %>% select(-line, -code) %>%
    mutate(item = item %>% str_replace_all("/[0-9]+/", "") %>%
              str_trim() %>% str_to_lower() %>%
              str_replace_all(' +', '.')
            ) %>%
    filter(item %in% c('farm', 'gross.domestic.product')) %>%
    pivot_longer(cols = -item, names_to = "year", values_to = "gdp") %>%
    mutate(year = str_replace(year, "^x", "") %>% as.integer()) %>%
    pivot_wider(names_from = "item", values_from = "gdp") %>%
    mutate(farm.prices = farm / gross.domestic.product)
  invisible(x)
}

get_farm_prices <- function(min.year = NA, max.year = NA) {
  # File is downloaded from https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx
  data_file <- file.path(data_dir, 'agriculture', 'Section1All_xls.xlsx')
  if (! file.exists(data_file)) {
    download.file("https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx",
                  data_file, mode="wb")
  }
  prices <- load_farm_data(data_file)
  if (! is.na(min.year)) prices <- prices %>% filter(year >= min.year)
  if (! is.na(max.year)) prices <- prices %<% filter(year <= max.year)
  prices <- prices %>% mutate(pct = 100 * farm.prices / max(farm.prices))
  invisible(prices)
}
