library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

load_pielke <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    if (exists('data_dir', envir = parent.frame())) {
      data_dir <- get('data_dir', envir = parent.frame())
    }
  }
  storms <- read_excel(file.path(data_dir, "storm_damages/Weinkle_normalized_hurricane_damages.xlsx"),
                       sheet = "Storm Damages") %>%
    janitor::clean_names() %>% mutate(year = as.integer(year)) %>%
    group_by(year) %>%
    summarize(base_damage = sum(base_economic_damage_us, na.rm = T),
              norm_damage = sum(normalized_pl_2018),
              storm_count = n(),
              major = sum(catgeory >= 3)) %>%
    ungroup()
  econ <- read_excel(file.path(data_dir, "storm_damages/Weinkle_normalized_hurricane_damages.xlsx"),
                     sheet = "Infaltion & Wealth") %>%
    janitor::clean_names() %>% mutate(year = as.integer(year))

  storms <- storms %>% left_join( select(econ, year, multiplier), by = "year") %>%
    mutate(const_dollar_damage = base_damage * multiplier)
  storms
}

load_grinsted <- function(data_dir) {
  weinkle <- read_excel(file.path(data_dir, "storm_damages/Grinsted_normalized_hurricane_damages.xls"),
                        sheet = "ATD of Weinkle") %>%
    janitor::clean_names() %>%
    mutate(year = year(lf_iso_time)) %>%
    group_by(year) %>%
    summarize(base_damage = sum(basedamage),
              norm_damage = sum(nd),
              storm_count = n(),
              atd = sum(atd)) %>%
    ungroup() %>%
    mutate(source = "Weinkle")

  icat <- read_excel(file.path(data_dir, "storm_damages/Grinsted_normalized_hurricane_damages.xls"),
                     sheet = "ATD of ICAT") %>%
    janitor::clean_names() %>%
    mutate(year = year(lf_iso_time)) %>%
    group_by(year) %>%
    summarize(base_damage = sum(basedamage),
              norm_damage = sum(nd),
              storm_count = n(),
              atd = sum(atd)) %>%
    ungroup() %>%
    mutate(source = "ICAT")

  ncei <- read_excel(file.path(data_dir, "storm_damages/Grinsted_normalized_hurricane_damages.xls"),
                     sheet = "ATD of NCEI") %>%
    janitor::clean_names() %>%
    mutate(year = year(lf_iso_time)) %>%
    group_by(year) %>%
    summarize(base_damage = sum(basedamage),
              storm_count = n(),
              atd = sum(atd)) %>%
    ungroup() %>%
    mutate(source = "NCEI")

  storms <- bind_rows(weinkle, icat, ncei)

  storms
}
