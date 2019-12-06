library(tidyverse)

if (! exists('prt')) source('lecture_utils.R')

plot_farm_prices <- function(data, min.year = NA, max.year = NA) {
  if (! is.na(min.year)) data <- data %>% filter(year >= min.year)
  if (! is.na(max.year)) data <- data %>% filter(year <= max.year)
  
  # trend in percent per year
  trend <- 100 * coef(lm(log(pct) ~ year, data = data))['year']
  
  p <- ggplot(data, aes(x = year, y = pct)) + 
    geom_point(size=3) + 
    geom_line() + 
    stat_smooth(method=lm, fill=NA) + 
    scale_x_continuous(breaks = seq(1950, 2015, 10)) + 
    scale_y_log10(breaks = seq(10,100,10), limits=c(10,105), expand=c(0,0)) + 
    annotate(geom="text", x = 1987, y = with(data, pct[year == 1987]) * 1.25, 
             hjust = 0, vjust = 0, 
             label=paste("Trend line:\n( = ", prt(trend, 1), "% per year)")) + 
    labs(x = "Year", 
         y = paste0("Farm prices / all prices (", with(data, year[which.max(farm.prices)]), " = 100)"), 
         title = with(data, paste0("Farm prices (", min(year), "-", max(year), ")")))
  p
}
