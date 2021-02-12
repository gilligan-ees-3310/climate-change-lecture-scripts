library(tidyverse)
library(rvest)

load_mei <- function() {
  # MEI version 2, 1079--present
  mei_text <- read_html("https://www.esrl.noaa.gov/psd/enso/mei/data/meiv2.data") %>%
    html_nodes("body") %>% html_text() %>%
    str_split("\n", simplify = TRUE) %>% keep(~str_detect(.x, "^[0-9]{4}")) %>%
    str_c(collapse = "\n")

  mei <- mei_text %>%
    read_table(col_names = c("year", month.abb),
             col_types = str_c(c("i", rep("d", 12)), collapse = ""),
             skip = 1) %>%
    gather(-year, key = month, value = mei) %>%
    mutate(month = ordered(month, levels = month.abb),
           date = year + (as.integer(month) - 1) / 12,
           mei = ifelse(mei < -900, NA, mei)) %>%
    arrange(date) %>%
    na.omit()

  invisible(mei)
}

load_old_mei <- function() {
  # Old version of MEI, 1950--2018
  mei_text <- read_html("https://www.esrl.noaa.gov/psd/enso/mei.old/table.html") %>%
    html_nodes("body") %>% html_text() %>%
    str_split("\n", simplify = TRUE) %>% keep(~str_detect(.x, "^[0-9]{4}")) %>%
    str_c(collapse = "\n")

  mei <- mei_text %>%
    read_tsv(col_names = c("year", month.abb),
               col_types = str_c(c("i", rep("d", 12)), collapse = "")) %>%
    gather(-year, key = month, value = mei) %>%
    mutate(month = ordered(month, levels = month.abb),
           date = year + (as.integer(month) - 1) / 12) %>%
    arrange(date) %>%
    na.omit()

  invisible(mei)
}

plot_mei <- function(mei, base_size = 10, limits = c(NA, NA),
                     break_interval = 10) {
  pos_fill = "red"
  pos_color = pos_fill
  weak_pos_fill = alpha("red", 0.2)
  weak_pos_color = alpha("red", 0.3)
  weak_neg_fill = alpha("blue", 0.2)
  weak_neg_color = alpha("blue", 0.3)
  neg_fill = "blue"
  neg_color = neg_fill

  mei %>% mutate(mei.pos = ifelse(mei > 0.5, mei, 0),
                 mei.weak_pos = ifelse(mei >= 0 & mei <= 0.5, mei, 0),
                 mei.weak_neg = ifelse(mei < 0 & mei >= -0.5, mei, 0),
                 mei.neg = ifelse(mei < -0.5, mei, 0)) %>%
    ggplot(aes(x = date)) +
    geom_area(aes(y = mei.neg), color = neg_color, fill = neg_fill,
              na.rm = TRUE) +
    geom_area(aes(y = mei.weak_neg), color = weak_neg_color,
              fill = weak_neg_fill, na.rm = TRUE) +
    geom_area(aes(y = mei.weak_pos), color = weak_pos_color,
              fill = weak_pos_fill, na.rm = TRUE) +
    geom_area(aes(y = mei.pos), color = pos_color, fill = pos_fill,
              na.rm = TRUE) +
    scale_x_continuous(breaks = seq(1950, 2030, break_interval),
                       limits = limits) +
    labs(x = "Year", y = "Multivariate El Nino Index") +
    theme_bw(base_size = base_size)
}
