#
# Deal with Kaya variables
#
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

this.year <- year(now())

fill_in <- function(df, this.year) {
  extrap <- function(x, y, xout) {
    df <- data.frame(x = x, y = y) %>% na.omit()
    if (nrow(df) >= 2) {
      yout = Hmisc::approxExtrap(df, xout = xout)
    } else {
      yout <- list(x = xout, y = rep_along(xout, NA))
    }
    invisible(yout)
  }

  interp.years <- seq(max(df %>% filter(year < this.year) %>% select(year)) + 1,
                      this.year)
  output <- data.frame(year = interp.years)
  nations <- df %>% select(nation, nat_code) %>% distinct()
  output <- output %>% crossing(nations)

  output <- bind_rows(df, output) %>% group_by(nation) %>%
    mutate_at(vars(-nation, -nat_code, -year),
              funs(ifelse(year %in% interp.years, extrap(year, ., year)$y, .))) %>%
    ungroup()
  invisible(output)
}

load_maddison_kaya <- function(data_dir = "data") {
  maddison <- read_excel(file.path(data_dir, "kaya", "mpd2018.xlsx"), "Full data")

  kaya <- maddison %>%
    select(nation = country, nat_code = countrycode, year, P = pop, g = rgdpnapc) %>%
    mutate(G = g * P,
           nation = str_to_title(nation) %>% str_trim() %>%
             str_replace_all(c("\\&" = "and",
                               "And" = "and", "Of" = "of", "The" = "the",
                               "Ussr" = "USSR",
                               "\\(.*\\)" = "",
                               "D\\.[Pp]\\.[Rr]\\. of Korea" = "North Korea",
                               "Republic of Korea" = "South Korea",
                               "Viet Nam" = "Vietnam",
                               "China, Hong Kong" = "Hong Kong",
                               "Libyan.*$" = "Libya",
                               "D.[Rr]. of [Tt]he Congo" = "Democratic Republic of the Congo",
                               "Lao People's Dr" = "Laos",
                               "Hong Kong.+$" = "Hong Kong",
                               "Taiwan,.*$" = "Taiwan",
                               "U.[Rr]. of Tanzania.*$" = "Tanzania",
                               "Former +" = "",
                               "C.te D'ivoire" = "Cote d'Ivoire",
                               "Guinea-Bissau" = "Guinea Bissau",
                               "Tfyr of Macedonia" = "Macedonia",
                               "State of Palestine" = "Palestine"
             )) %>% str_trim())

  kaya_world <- kaya %>% group_by(year) %>%
    summarize(P = sum(P, na.rm = T), G = sum(G, na.rm = T)) %>%
    ungroup() %>% mutate(g = G / P, nation = "World", nat_code = "WLD")

  kaya <- bind_rows(kaya, kaya_world)


  cdiac_global_filename <- "global.1751_2014.csv"
  cdiac_national_filename <- "nation.1751_2014.csv"

  co2.nat.names <- read_csv(file.path(data_dir, "kaya", cdiac_national_filename),
                            col_names = FALSE, n_max = 1) %>%
    as_vector() %>% unname()

  co2.nat <- read_csv(file.path(data_dir, "kaya", cdiac_national_filename),
                      col_names = co2.nat.names, skip = 4,
                      na = c("", "na", "n/a", "NA", "N/A", ".")) %>%
    clean_names() %>%
    filter(! is.na(year)) %>%
    select(nation, year, F = starts_with("total"), efg = starts_with("per_capita")) %>%
    mutate(nation = str_to_title(nation) %>% str_trim() %>%
             str_replace_all(c("\\&" = "and",
                               "And" = "and", "Of" = "of", "The" = "the",
                               "Ussr" = "USSR",
                               "\\(.*\\)" = "",
                               "Islamic Republic of" = "",
                               "Republic of Korea" = "South Korea",
                               "United States of America" = "United States",
                               "Hong Kong.+$" = "Hong Kong",
                               "Democratic People S South Korea" = "South Korea",
                               "Democratic Republic of Vietnam" = "Vietnam",
                               "Viet Nam" = "Vietnam",
                               "Libyan.+$" = "Libya",
                               "Lao People S Democratic Republic" = "Laos",
                               "United Republic of Tanzania" = "Tanzania",
                               "Plurinational State of Bolivia" = "Bolivia",
                               "Cote D Ivoire" = "Cote d'Ivoire",
                               "Occupied Palestinian Territory" = "Palestine"
             )) %>% str_trim())

  co2.glob.names <- read_csv(file.path(data_dir, "kaya", cdiac_global_filename),
                            col_names = FALSE, n_max = 1) %>%
    as_vector() %>% unname()

  co2.glob <- read_csv(file.path(data_dir, "kaya", cdiac_global_filename),
                      col_names = co2.glob.names, skip = 2,
                      na = c("", "na", "n/a", "NA", "N/A", ".")) %>%
    clean_names() %>%
    filter(!is.na(year)) %>%
    mutate(nation = "World")

  co2.fuel <- co2.glob %>%
    select(year, nation,
           oil = contains("liquid_fuel"),
           gas = contains("gas_fuel"),
           coal = contains("solid_fuel"),
           cement = contains("from_cement_production"),
           flaring = contains("gas_flaring")) %>%
    mutate(gas = gas + flaring) %>%
    select(-flaring)

  co2.glob <- co2.glob %>%
    select(year, nation, F = starts_with("total"), efg = starts_with("per_capita"))

  co2.nat <- co2.nat %>% bind_rows(co2.glob)

  kaya <- kaya %>% inner_join(co2.nat, by = c("year","nation")) %>%
    fill_in(this.year)

  list(kaya = kaya, co2.fuel = co2.fuel) %>%
    invisible()
}
