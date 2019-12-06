library(tidyverse)
library(stringr)

extract_years <- function(giss_data, year.end = 'dec') {
  ma <- tolower(month.abb)
  year.end <- tolower(year.end)
  target_month <- ma[(which(ma == year.end) - 6 + 11) %% 12 + 1]
  annual_data <- giss_data %>% filter(month == target_month)
  invisible(annual_data)
}

giss_baseline <- function(monthly_data, baseline_years = c(1951,1980)) {
  bl <- (monthly_data %>% filter(year >= min(baseline_years) & year <= max(baseline_years)) %>%
           summarize(baseline = mean(t.anom, na.rm=T)))$baseline[[1]]
}

giss_range <- function(giss_data, start_year = NA, stop_year = NA, year.end = 'dec', baseline_years = c(1951,1980)) {
  monthly_data <- giss_data$data
  bl = giss_baseline(monthly_data, baseline_years)
  monthly_data <- monthly_data %>%
    mutate(t.anom = t.anom - bl, t.anom.annual = t.anom.annual - bl,
           t.anom.decadal = t.anom.decadal - bl)
  if (is.null(year.end) || is.na(year.end)) {
    year.end <- tail(monthly_data$month[! is.na(monthly_data$t.anom)],1)
  }
  annual_data <- extract_years(monthly_data, year.end) %>% filter(! is.na(t.anom.annual))
  if (!is.na(start_year)) {
  annual_data <- annual_data %>% filter(year >= start_year)
  }
  if(! is.na(stop_year)) {
    annual_data <- annual_data %>% filter(year <= stop_year)
  }
  range(annual_data$t.anom.annual)
}

hiatus_plot <- function(data, plt.index, min_year = 1970, change_year = 1998,
                        last_year = NA, year.end = 'dec',
                        baseline_years = c(1951,1980)) {
    if (! plt.index %in% 1:6) stop("Invalid plot index ", plt.index)

  data.source.text <- data$label
  monthly_data <- data$data
  bl = giss_baseline(monthly_data, baseline_years)
  monthly_data <- monthly_data %>%
    mutate(t.anom = t.anom - bl, t.anom.annual = t.anom.annual - bl,
           t.anom.decadal = t.anom.decadal - bl)
  if (is.null(year.end) || is.na(year.end)) {
    year.end <- tail(monthly_data$month[! is.na(monthly_data$t.anom)],1)
  }
  annual_data <- extract_years(monthly_data, year.end) %>% filter(! is.na(t.anom.annual))
  annual_data <- annual_data %>% filter(year >= min_year)
  early <- annual_data %>% filter(year <= change_year)
  if (nrow(early) > 0) {
    early$time.frame <- 'early'
    min.early <- min(early$year)
    max.early <- max(early$year)
  } else {
    min.early <- -1
    max.early <- -1
  }

  late <- annual_data %>% filter(year >= change_year)
  if (nrow(late) > 0) {
    late$time.frame <- 'late'
  }

  annual_data <- rbind(early, late)
  max.year <- ceiling(max(annual_data$time))
  min.temp <- min(annual_data$t.anom.annual)
  chg.temp <- min(late$t.anom.annual)

  fit.label <- paste0("Linear trend for ", min.early, "-", max.early)
  plot.credit.label <- "Adapted from Grant Foster, https://tamino.wordpress.com/2015/01/20/its-the-trend-stupid-3/"

  if (! is.na(last_year)) {
    annual_data <- annual_data %>% filter(year <= last_year)
  }

  if (plt.index %in% c(1,2,6)) {
    plt_data <- annual_data
  } else {
    plt_data <- early
  }
  if (plt.index == 2) {
    early_color = "light steel blue2"
    late_color = "dark red"
  } else {
    early_color = "dark blue"
    late_color = "dark red"
  }

  p <- ggplot(plt_data, aes(time, t.anom.annual, color = time.frame)) +
    geom_point(size=I(4)) +
    geom_line(size=I(1))

  if (plt.index == 4) {
    p <- p + geom_smooth(data = early, method=lm, se=TRUE, fullrange=FALSE)
  } else if (plt.index > 4) {
    p <- p + geom_smooth(data = early, method=lm, se=TRUE, fullrange=TRUE)
  }
  p <- p +
    scale_color_manual(values = c(early = early_color, late = late_color), guide = 'none') +
    labs(y=substitute(paste("Temperature Anomaly:  ",degree*C,
                            "   (",base.start-base.end," Baseline)"),
                      list(base.start = baseline_years[1], base.end = baseline_years[2])),
         x = "Year")
  ym <- mean(annual_data$t.anom.annual, na.rm=T)
  yv <- ym + c(1.1, 1.5) * range(annual_data$t.anom.annual - ym, na.rm=T)
  p <- p + xlim(min_year,max.year) + ylim(yv)
  p <- p +  annotate("text",x=max.year,y=0.8 *
               min.temp, label=data.source.text,
             color="dark gray", hjust=1, size=5) +
    annotate("text",x=max.year,y= 1.2 * min.temp, label=plot.credit.label,
             color="dark gray", hjust=1, size=5)
  if (plt.index >= 4) {
    p <- p +
      annotate("text", x=max.year,y=chg.temp * 0.9, label=fit.label,
               hjust=1, vjust=1, size=7, color="dark blue")
  }

  p <- p + theme_bw(base_size=20)

  p
}

