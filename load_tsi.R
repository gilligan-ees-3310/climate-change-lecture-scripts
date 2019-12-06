tsi.file <- file.path(data.dir, 'solar', 'historical_tsi.csv')

load_tsi <- function(as.anomaly = TRUE) {
  runmean <- function(x, window) {
    h = ceiling((window  - 1)/2)
    l = floor((window  - 1)/2)
    offset = seq(-l, h)
    indices = seq(l + 1, length(x) - h)
    y <- unlist(lapply(indices,
                       function(i) mean(x[i + offset])))
    z = rep_len(NA, length(x))
    z[indices] <- y
    invisible(z)
  }

  tsi <- read_csv(tsi.file)
  names(tsi) <- c('year','value')
  if (as.anomaly) {
    tsi <- tsi %>% mutate(t.anom.annual = value - mean(value))
  } else {
    tsi <- tsi %>% mutate(t.anom.annual = value)
  }
  tsi <- tsi %>% select(-value)
  tsi$t.anom.decadal <- runmean(tsi$t.anom.annual, 10)
  tsi$var = "Total Solar Irradiance"
  invisible(tsi)
}
