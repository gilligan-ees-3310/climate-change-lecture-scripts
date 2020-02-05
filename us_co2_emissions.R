library(tidyverse)
library(stringr)
library(lubridate)

get_us_emissions <- function(data_dir = data_dir) {
  download.file("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T12.01",
                file.path(data_dir, 'MET_T12_01.csv'))
}
  
plot_us_co2_emissions <- function(data_dir = file.path(data_dir, 'ghg_emissions'), 
                                  file = 'MET_T12_01.csv') {
  emissions <- read_csv(file.path(data_dir, file)) %>%
    dplyr::filter(MSN == 'TETCEUS') %>% mutate(year = YYYYMM %/% 100, month = YYYYMM %% 100) %>%
    dplyr::filter(month == 13)
  
  ggplot(emissions, aes(x = year, y = Value)) + 
    geom_line(size = 1) +  geom_point(size = 3) +
    annotate(geom = 'segment', x = c(1980, 1982), y = c(5500, 5250), 
               xend = c(1973.2, 1979.2), yend = c(4755,4995), 
               arrow = arrow(type='closed', angle=20, length=unit(10, 'pt'))) + 
    annotate(geom="label", x = c(1980, 1985), y = c(5500, 5250), 
             label = c("Arab oil embargo", "Iran revolution"), 
             hjust = 0.5, vjust = 0.5, size = 5) +
    labs(x = "Year", y = expression(MMT~CO[2]), title = expression(paste("U.S. Energy-Related ", CO[2], " Emissions"))) +
    theme_bw(base_size = 20)
}
