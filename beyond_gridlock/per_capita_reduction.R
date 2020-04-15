library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2)

x <- spread(rcp_45 %>%
              filter(Variable %in% c('Emissions|CO2|Fossil fuels and Industry', 'Population|Total')) %>%
              select(Year, Variable, value, Region), key = "Variable", value = "value")
names(x) <- c('year', 'region', 'co2', 'pop')
x <- x %>% group_by(year) %>% summarize(co2 = sum(co2), pop = sum(pop), co2_pc = sum(co2)/sum(pop))
y <- spread(rcp_85 %>% filter(Variable %in% c('Emissions|CO2|Fossil fuels and Industry', 'Population|Total')) %>% select(Year, Variable, value, Region), key = "Variable", value = "value")
names(y) <- c('year', 'region', 'co2', 'pop')
y <- y %>% group_by(year) %>% summarize(co2 = sum(co2), pop = sum(pop), co2_pc = sum(co2)/sum(pop))
x$scenario <- 'RCP 4.5'
y$scenario <- 'RCP 8.5'
z <- rbind(x,y)

p_co2 <- ggplot(z, aes(x = year, y = co2_pc, color = scenario)) + geom_line() + geom_point() +
  labs(x = "Year", y = "Per-capita emissions (tons per year)") +
  ylim(0, NA) +
  theme_bw(base_size=20)

x$delta.co2_pc <- y$co2_pc - x$co2_pc

p_reduction <- ggplot(x, aes(x = year, y = delta.co2_pc)) + geom_line() + geom_point(size=2) +
  labs(x = "Year", y = "Per-capita emissions reduction (tons per year)") +
  ylim(0, NA) +
  theme_bw(base_size=20)

print(p_co2)
print(p_reduction)
