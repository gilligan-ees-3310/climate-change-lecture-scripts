library(tidyverse)
library(stringr)
library(rstan)
library(shinystan)

source('load_giss.R')

rstan_options(auto_write = TRUE)
theme_set(theme_bw())


extract_years <- function(giss_data, year.end = 'dec') {
  ma <- tolower(month.abb)
  year.end <- tolower(year.end)
  target_month <- ma[(which(ma == year.end) - 6 + 11) %% 12 + 1]
  annual_data <- giss_data %>% filter(month == target_month)
  invisible(annual_data)
}

prepare_data <- function(min_year = 1970, change_year =  1998, last_year = NA, 
                         year.end = 'dec') {
  giss_data <- load_giss_data();
  
  monthly_data <- giss_data$data
  if (is.null(year.end) || is.na(year.end)) {
    year.end <- tail(monthly_data$month[! is.na(monthly_data$t.anom)],1)
  }
  annual_data <- extract_years(monthly_data, year.end) %>% filter(! is.na(t.anom.annual))
  annual_data <- annual_data %>% filter(year >= min_year)
  early <- annual_data %>% filter(year <= change_year)
  late <- annual_data %>% filter(year >= change_year)
  early$time.frame <- 'early'
  late$time.frame <- 'late'
  annual_data <- rbind(early, late)
  
  invisible(list(annual = annual_data, early = early, late = late, label = giss_data$label))
}


stan_giss <- function(data, script.dir = NULL) {
  stan_data <- list(N = nrow(data), year = data$year, temp = data$t.anom.annual)
  model_file <- 'giss.stan'
  if (! is.null(script.dir)) model_file <- file.path(script.dir, model_file)
  
  model <- stan_model(model_file, 'Linear regression of GISS Temperature')
  
  fit <- sampling(model, stan_data, cores = 4)
  
  fit.df <- as.data.frame(fit)
  
  mean_year <- mean(stan_data$year)
  mean_temp <- mean(stan_data$temp)
  sd_year <- sd(stan_data$year)
  sd_temp <- sd(stan_data$temp)
  
  invisible(list(fit = fit, df = fit.df))
}


stan_giss2 <- function(data) {
  stan_data <- list(N = nrow(data), year = data$year, temp = data$t.anom.annual)
  
  model <- stan_model('giss2.stan', 'Linear regression of GISS Temperature')
  
  fit <- sampling(model, stan_data, cores = 4)
  
  fit.df <- as.data.frame(fit)
  
  invisible(list(fit = fit, df = fit.df))
}


stan_hiatus_plot <- function(data, fit, plt.index, min_year = 1970, change_year = 1998, 
                        last_year = NA, year.end = 'dec',
                        n.sample = 500, sample.alpha = 0.01, 
                        base.size = 20, a.scale = 0.5,
                        sample.line.size = 1,
                        point.size = 4, line.size = 1) {
  if (! plt.index %in% 1:5) stop("Invalid plot index ", plt.index)

  data.source.text <- data$label
  early <- data$early
  late <- data$late
  annual_data <- data$annual
    
  min.early <- min(early$year)
  max.early <- max(early$year)
  max.year <- ceiling(max(annual_data$time))
  min.temp <- min(annual_data$t.anom.annual)
  chg.temp <- min(late$t.anom.annual)
  
  fit.label <- paste("Linear fit to ",min.early, "-", max.early, sep='')
  plot.credit.label <- "Adapted from Grant Foster, https://tamino.wordpress.com/2015/01/20/its-the-trend-stupid-3/"
  
  if (! is.na(last_year)) {
    annual_data <- annual_data %>% filter(year <= last_year)
  }
  
  if (plt.index %in% c(1,5)) {
    plt_data <- annual_data
  } else {
    plt_data <- early
  }
  
  p <- ggplot(plt_data, aes(time, t.anom.annual, color = time.frame))
  
  if (plt.index == 3) {
    year_low <- min(plt_data$year)
    year_high <- max(plt_data$year)
    p <- p + geom_segment(data = sample_n(fit$df, n.sample), 
                         substitute(aes(y = slope * year_low + intercept, 
                             yend = slope * year_high + intercept, 
                         x = year_low, xend = year_high),
                         list(year_low = force(year_low), year_high = force(year_high))),
                         alpha = sample.alpha, color = 'darkgreen', 
                         size = I(sample.line.size))
  } else if (plt.index > 3) {
    p <- p + geom_abline(data = sample_n(fit$df, n.sample), 
                         aes(intercept = intercept, slope = slope), 
                         alpha = sample.alpha, color = 'darkgreen', 
                         size = I(sample.line.size))
  }
  p <- p + 
    geom_point(size=I(point.size)) + 
    geom_line(size=I(line.size))
  p <- p + 
    scale_color_manual(values = c(early = 'darkblue', late = 'darkred'), guide = 'none') +
    labs(y=expression(paste("Temperature Anomaly:  ",degree*C,"   (",1951-1980," Baseline)")),
         x = "Year") + 
    xlim(min_year,max.year) + ylim(1.25 * range(annual_data$t.anom.annual)) + 
    annotate("text",x=max.year,y=0.8 * min.temp, label=data.source.text, 
             color="darkgray", hjust=1, size= base.size * a.scale * 0.5) +
    annotate("text",x=max.year,y= 1.2 * min.temp, label=plot.credit.label, 
             color="darkgray", hjust=1, size= base.size * a.scale * 0.5)
  if (plt.index >= 3) {
    p <- p + 
      annotate("text", x=max.year,y=chg.temp * 0.9, label=fit.label, 
               hjust=1, vjust=1, size= base.size * a.scale * 0.7, 
               color="darkgreen")
  }
  
  p <- p + theme_bw(base_size=base.size)
  
  p
}

lm_hiatus_plot <- function(data, plt.index, min_year = 1970, change_year = 1998, 
                        last_year = NA, year.end = 'dec', 
                        base.size = 20, a.scale = 0.5,
                        line.size = 1, point.size = 4) {
  if (! plt.index %in% 1:5) stop("Invalid plot index ", plt.index)
  
  data.source.text <- data$label
  early <- data$early
  late <- data$late
  annual_data <- data$annual
  
  min.early <- min(early$year)
  max.early <- max(early$year)
  max.year <- ceiling(max(annual_data$time))
  min.temp <- min(annual_data$t.anom.annual)
  chg.temp <- min(late$t.anom.annual)
  
  fit.label <- paste("Linear fit to ",min.early, "-", max.early, sep='')
  plot.credit.label <- "Adapted from Grant Foster, https://tamino.wordpress.com/2015/01/20/its-the-trend-stupid-3/"
  
  if (! is.na(last_year)) {
    annual_data <- annual_data %>% filter(year <= last_year)
  }
  
  if (plt.index %in% c(1,5)) {
    plt_data <- annual_data
  } else {
    plt_data <- early
  }
  
  p <- ggplot(plt_data, aes(time, t.anom.annual, color = time.frame))
  
  if (plt.index == 3) {
    p <- p + geom_smooth(data = early, method=lm, se=TRUE, fullrange=FALSE, color = "darkgreen")
  } else if (plt.index > 3) {
    p <- p + geom_smooth(data = early, method=lm, se=TRUE, fullrange=TRUE, color = "darkgreen")
  }
  p <- p + 
    geom_point(size=I(point.size)) + 
    geom_line(size=I(line.size))
  p <- p +
  scale_color_manual(values = c(early = 'darkblue', late = 'darkred'), guide = 'none') +
    labs(y=expression(paste("Temperature Anomaly:  ",degree*C,"   (",1951-1980," Baseline)")),
         x = "Year") + 
    xlim(min_year,max.year) + ylim(1.25 * range(annual_data$t.anom.annual)) + 
    annotate("text",x=max.year,y=0.8 * min.temp, label=data.source.text, 
             color="darkgray", hjust=1, size=base.size * a.scale * 0.5) +
    annotate("text",x=max.year,y= 1.2 * min.temp, label=plot.credit.label, 
             color="darkgray", hjust=1, size=base.size * a.scale * 0.5)
  if (plt.index >= 3) {
    p <- p + 
      annotate("text", x=max.year,y=chg.temp * 0.9, label=fit.label, 
               hjust=1, vjust=1, size= base.size * a.scale * 0.7, 
               color="darkgreen")
  }
  
  p <- p + theme_bw(base_size=base.size)
  
  p
}

