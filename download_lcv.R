library(httr)
library(tidyverse)
library(janitor)

lcv_dir <- file.path(data_dir, "politics", "lcv_scores")

lcv_file_template <- "{year}-{chamber}-scorecard-grid-export.csv"

download_lcv <- function(min_year = NA, max_year = NA, dest_dir = lcv_dir) {
  url_base <- "http://scorecard.lcv.org/exports/"

  if (is.na(min_year)) min_year <- 1972
  if (is.na(max_year)) max_year <- 2018

  for (year in seq(min_year, max_year)) {
    for (chamber in c("senate", "house")) {
      fname <- str_glue_data(list(year = year, chamber = chamber),
                             lcv_file_template)
      url <- parse_url(url_base)
      url$path <- file.path(url$path, fname) %>% str_replace_all("//+", "/")
      surl <- build_url(url)
      fpath <- file.path(dest_dir, fname)
      if (!file.exists(fpath)) {
        message("Downloading ", surl, " to ", fpath)
        download.file(surl, destfile = fpath)
      }
    }
  }
}

load_lcv <- function(src_dir = lcv_dir) {
  data <- tibble()

  for (chamber in c("senate", "house")) {
    for (year in (1972:2018)) {
      fname <- str_glue_data(list(year = year, chamber = chamber),
                             lcv_file_template)
      fpath <- file.path(src_dir, fname)
      var <- as.name(str_c("x", year, "_score"))
      df <- read_csv(fpath, skip = 6) %>% clean_names()
      if (chamber == "house") {
        df <- df %>%       select(district:lifetime_score)
      } else {
        df <- df %>% select(state:lifetime_score)
      }
      df <- df %>% mutate(score = as.numeric(!!var)) %>%
        group_by(party) %>% summarize(score = mean(score, na.rm = T)) %>%
        ungroup() %>%
        mutate(chamber = chamber, year = year)
      data <- bind_rows(data, df)
    }
  }
  invisible(data)
}

plot_lcv <- function(df, base_size = 20) {
  df %>% filter(party %in% c("D", "R")) %>%
    mutate(chamber = factor(chamber), party = factor(party)) %>%
    ggplot(aes(x = year, y = score, color = party, linetype = chamber)) +
    geom_vline(xintercept = 1990, color = "dark gray") +
    geom_line(size = 1) +
    scale_color_manual(values = c(D = "dark blue", R = "dark red"),
                       labels = c(D = "Democrats", R = "Republicans"),
                       name = "Party") +
    scale_linetype_manual(values = c(senate = 2, house = 1),
                          labels = c(house = "House", senate = "Senate"),
                          name = "Chamber") +
    labs(x = "Year", y = "LCV Score",
         title = "League of Conservation Voters scores, by party and chamber") +
    theme_bw(base_size = base_size) +
    theme(legend.position = c(1, 0.5), legend.justification = c(1, 0.5),
          legend.key.width = unit(3, "lines"),
          plot.margin = unit(c(1.0, 1.0, 0.5, 0.5), "char")) -> p

  p
}
