library(pacman)
p_load(tidyverse, magrittr, lubridate, readxl, broom, janitor, censusapi)
p_load_gh("walkerke/tidycensus")

if (! exists("data_dir")) {
  data_dir <- rprojroot::find_root(rprojroot::has_file("semester.yml")) %>%
    file.path("data")

}

elec_sales_file <- file.path(data_dir, "energy", "sales_annual.xlsx")
elec_sales_url <- "https://www.eia.gov/electricity/data/state/sales_annual.xlsx"

egrid_emission_file <- file.path(data_dir, "energy", "egrid2016_summarytables.xlsx")
egrid_emission_url <-
  "https://www.epa.gov/sites/production/files/2018-02/egrid2016_summarytables.xlsx"

download_energy_data <- function(force = FALSE) {

  if (force || ! file.exists(elec_sales_file)) {
    download.file(elec_sales_url, elec_sales_file, mode = "wb")
  }
  if (force || ! file.exists(egrid_emission_file)) {
    download.file(egrid_emission_url, egrid_emission_file, mode = "wb")
  }

}


make_use_data <- function() {
  # Make a tibble of state FIPS codes. Add `00` for the US.
  state_fips <- fips_codes %>% select(state, state_code, state_name) %>%
    distinct() %>%
    bind_rows(tibble(state = "US", state_code = "00",
                     state_name = "United States"))

  use_data <- read_excel(elec_sales_file, skip = 2,
                         na = c("", "NA"),
                         col_names = c("year", "state", "sector",
                                       "residential", "commercial", "industrial",
                                       "transportation", "other", "total"),
                         col_types = c("numeric", "text", "text",
                                       rep("numeric", 6))) %>%
    mutate(sector = factor(sector)) %>% left_join(state_fips, by = "state")

  # Divide emission factors by 2200 to convert from pounds per MWh to
  # metric tons per MWh
  emission_factors <- read_excel(egrid_emission_file, "Table 3", skip = 3) %>%
    clean_names(case = "snake") %>% dplyr::rename(state = x1) %>%
    mutate(e_factor = co2e / 2200) %>% filter(! is.na(state))

  use_data <- use_data %>%
    left_join(select(emission_factors, state, e_factor), by = "state") %>%
    mutate(emissions = residential * e_factor)

  invisible(use_data)
}

get_state_pop_1990s <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    if (exists('data_dir', envir = parent.frame())) {
      data_dir <- get('data_dir', envir = parent.frame())
    }
  }
  state_pop_1990s_file <- file.path(data_dir, "energy", "state_pop_1990s.Rds")

  if (file.exists(state_pop_1990s_file)) {
    state_pop_1990s <- read_rds(state_pop_1990s_file)
  } else {
    county_pop_1990s <- map_df(fips,
                               ~getCensus("pep/int_charagegroups", 1990,
                                          vars=c("POP", "YEAR", "STATE",
                                                 "COUNTY", "AGEGRP",
                                                 "RACE_SEX", "HISP"),
                                          region="county:*",
                                          regionin=str_c("state:", .x))
    ) %>%
      as_tibble() %>% clean_names(case = "snake") %>%
      mutate(pop = as.numeric(pop))

    # YEAR is two digit, so add 1900
    state_pop_1990s <- county_pop_2 %>%
      group_by(year, state) %>% summarize(pop = sum(pop)) %>% ungroup() %>%
      mutate(year = 1900 + as.integer(year)) %>%
      filter(year >= 1990) %>% dplyr::rename(state_code = state)

    write_rds(state_pop_1990s, state_pop_1990s_file)
  }

  sp_1990 <- state_pop_1990s

  invisible(sp_1990)
}

get_state_pop_2000s <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    if (exists('data_dir', envir = parent.frame())) {
      data_dir <- get('data_dir', envir = parent.frame())
    }
  }
  state_pop_2000s_file <- file.path(data_dir, "energy", "state_pop_2000s.Rds")

  if (file.exists(state_pop_2000s_file)) {
    state_pop_2000s <- read_rds(state_pop_2000s_file)
  } else {
    state_pop_2000s <- getCensus("pep/int_charagegroups", 2000,
                                 vars=c("POP", "DATE_", "DATE_DESC", "GEONAME"),
                                 region="state:*") %>%
      as_tibble() %>% clean_names(case = "snake")
    write_rds(state_pop_2000s, state_pop_2000s_file)
  }

  sp_2000 <- state_pop_2000s %>%
    mutate(date = date_desc %>% str_replace_all("([0-9]) *[A-Za-z].*$", "\\1") %>% mdy) %>%
    filter(date != "2000-04-01") %>%
    mutate(year = year(date), pop = as.numeric(pop)) %>%
    select(year, state_code = state, pop)

  invisible(sp_2000)
}

get_state_pop_2010s <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    if (exists('data_dir', envir = parent.frame())) {
      data_dir <- get('data_dir', envir = parent.frame())
    }
  }
  state_pop_2010s_file <- file.path(data_dir, "energy", "state_pop_2010.Rds")

  if (file.exists(state_pop_2010s_file)) {
    state_pop_2010s <- read_rds(state_pop_2010s_file)
  } else {
    state_pop_2010s <- get_estimates("state", "population",
                                     time_series = TRUE) %>%
      clean_names(case = "snake") %>% dplyr::rename(state = geoid)
    write_rds(state_pop_2010s, state_pop_2010s_file)
  }

  sp_2010 <- state_pop_2010s %>% filter(date >= 3) %>%
    mutate(year = 2010 + date - 3, date = make_date(year, 7, 1)) %>%
    spread(key = variable, value = value) %>% clean_names(case = "snake") %>%
    select(year, state_code = state, pop)
  invisible(sp_2010)
}

get_state_pop <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    if (exists('data_dir', envir = parent.frame())) {
      data_dir <- get('data_dir', envir = parent.frame())
    }
  }
  sp_1990 <- get_state_pop_1990s(data_dir)
  sp_2000 <- get_state_pop_2000s(data_dir)
  sp_2010 <- get_state_pop_2010s(data_dir)

  sp <- bind_rows(sp_1990, sp_2000, sp_2010) %>%
    filter(state_code != "00")

  sp_us <- sp %>% group_by(year) %>%
    summarize(state_code = "00", pop = sum(pop)) %>%
    ungroup()

  sp <- bind_rows(sp, sp_us)
  invisible(sp)
}

get_per_capita_energy_use <- function(use_data, state_pop) {
  per_capita_use <- use_data %>% filter(sector == "Total Electric Industry") %>%
    left_join(state_pop, by = c("state_code", "year")) %>%
    mutate(per_capita_use = residential / pop,
           per_capita_emissions = emissions / pop)
  invisible(per_capita_use)
}

get_us_energy_use <- function(per_capita_use) {
  us_total <- per_capita_use %>% filter(state != "US") %>%
    group_by(year, sector) %>%
    summarize(residential = sum(residential),
              emissions = sum(emissions),
              pop = sum(pop),
              per_capita_use = residential / pop,
              per_capita_emissions = emissions / pop) %>%
    ungroup()
  invisible(us_total)
}

res_elec_trends <- function(us_energy_use) {
  fits <- us_energy_use %>%
    mutate(early = lm(per_capita_use ~ year, data = .,
                      subset = year < 2007) %>%
             augment(newdata = tibble(year = year)) %$% .fitted,
           full = lm(per_capita_use ~ poly(year, 3), data = .) %>%
             augment(newdata = tibble(year = year)) %$% .fitted) %>%
    select(year, early, full) %>%
    pivot_longer(cols=-year, names_to = "key",
                 values_to = "per_capita_use") %>%
    mutate(Fit = factor(key, levels = c("early", "full"),
                        labels = c("Linear extrapolation from pre-2007 data",
                                   "Cubic polynomial fit to all data")))
  fits
}

delta_consumption <- function(us_energy_use, fits = NULL) {
  us_pop_latest <- us_energy_use %>% top_n(1, year) %$% pop

  if (is.null(fits)) {
    fits <- res_elec_trends(us_energy_use)
  }

  delta_consumption_us <- fits %>%
    pivot_wider(id_cols = year, names_from = key, values_from=per_capita_use) %>%
    mutate(delta = early - full) %>% arrange(year) %>%
    top_n(1, year) %$% delta * us_pop_latest

}

delta_emissions <- function(per_capita_energy_use) {
  emissions_change <- per_capita_energy_use %>% filter(state != "US") %>%
    select(year, state, sector, per_capita_use, e_factor, pop) %>%
    group_by(state, sector) %>%
    mutate(early = lm(per_capita_use ~ year, data = ., subset = year < 2007) %>%
             augment(newdata = tibble(year = year)) %$% .fitted,
           full = lm(per_capita_use ~ poly(year, 3), data = .) %>%
             augment(newdata = tibble(year = year)) %$% .fitted,
           delta = early - full,
           delta_total = pop * delta) %>%
    top_n(1, year) %>%
    mutate(delta = delta * e_factor, delta_total = delta_total * e_factor) %>%
    ungroup() %>%
    group_by(sector) %>%
    summarize(delta_total = sum(delta_total), pop = sum(pop),
              delta_per_capita = delta_total / pop) %>%
    ungroup()
  invisible(emissions_change)
}

plot_res_electricity <- function(us_energy_use, fits = NULL) {
  if (is.null(fits)) {
    fits <- res_elec_trends(us_energy_use)
  }

  line_levels <- levels(fits$Fit)

  line_values <- set_names(c("dashed", "solid"), line_levels)

  p <- ggplot(us_energy_use, aes(x = year, y = per_capita_use)) +
    geom_point() +
    geom_line(aes(color = Fit, linetype = Fit), data = fits, size = 1) +
    scale_color_brewer(palette = "Dark2", name = NULL) +
    scale_linetype_manual(values = line_values, name = NULL) +
    labs(x = "Year",
         y = "MWh per year",
         title = "Per Capita Electricity Consumption") +
    theme_bw(base_size = 16) +
    theme(legend.position = c(0.07, 0.9),
          legend.justification = c(0,0.5),
          legend.key.height = unit(0.5, "lines"),
          legend.key.width = unit(2.5, "lines"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.title = element_blank())

  plot(p)
}
