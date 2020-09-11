library(tidyverse)
library(tsibble)
library(somebm)

gen_series <- function(n_years = 100, months = 12, hurst = c(0.7, 0.9)) {
  bm <- bm(n = n_years * months) %>%
    as_tsibble() %>%
    mutate(year = n_years * index, name = "Brownian") %>%
    as_tibble()

  fbm <- map(hurst,
             ~fbm(.x, n_years * months) %>%
               as_tsibble() %>%
               mutate(year = n_years * index,
                      name = str_c("Fractional ", .x)) %>%
               as_tibble()
  )
  df <- bind_rows(bm, fbm)
  invisible(df)
}

doit <- function(n_years, hurst = c(0.7, 0.9)) {
  seed <- lubridate::now() %>% as.integer()
  set.seed(seed)
  df <- gen_series(n_years, hurst = hurst)
  p <- ggplot(df, aes(x = year, y = value, color = name)) +
    geom_line(size = 1) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw()
  print(p)
  invisible(list(seed = seed, df = df))
}
