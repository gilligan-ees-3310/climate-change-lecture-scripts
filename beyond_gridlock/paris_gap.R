library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(broom)
library(ggplot2)

emissions <- data.frame(year = c(2010,2020,2025,2030,2035,2050),
                        baseline = c(NA,NA,61,65,NA,NA),
                        policy = c(NA,NA,57,60,NA,NA),
                        indc_u = c(NA,NA,54,56,NA,NA),
                        indc_c = c(NA,NA,53,54,NA,NA),
                        baseline_min = c(NA,NA,57,64,NA,NA),
                        policy_min = c(NA,NA,55,58,NA,NA),
                        indc_u_min = c(NA,NA,53,54,NA,NA),
                        indc_c_min = c(NA,NA,52,52,NA,NA),
                        baseline_max = c(NA,NA,64,70,NA,NA),
                        policy_max = c(NA,NA,58,62,NA,NA),
                        indc_u_max = c(NA,NA,58,59,NA,NA),
                        indc_c_max = c(NA,NA,56,57,NA,NA),
                        lim_2 = c(NA,52,48,42,NA,23),
                        lim_2_min = c(NA,49,46,31,NA,18),
                        lim_2_max = c(NA,53,50,44,NA,27)
)

e_med <- emissions %>% select(-ends_with("_min"), -ends_with("_max")) %>%
  gather(key = scenario, value = emissions, -year)
e_min <- emissions %>% select(year, ends_with('_min')) %>%
  gather(key = scenario, value = emissions_min, -year) %>%
  mutate(scenario = str_replace(scenario, "_min", ""))
e_max <- emissions %>% select(year, ends_with('_max')) %>%
  gather(key = scenario, value = emissions_max, -year) %>%
  mutate(scenario = str_replace(scenario, "_max", ""))
e <- e_med %>% left_join(e_min) %>% left_join(e_max) %>% na.omit() %>%
  filter(year >= 2025 & year <= 2035) %>%
  mutate(scenario = ordered(scenario, levels = c('baseline', 'policy', 'indc_u', 'indc_c', 'lim_2'),
                            labels = c('Baseline', 'Current policy trajectory', 'Unconditional INDC', 'Conditional INDC', '<2C warming')))

p <- ggplot(e, aes(x = year, y = emissions, ymin = emissions_min, ymax = emissions_max, color = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.1) + geom_line() + geom_point() +
  scale_color_brewer(type="qual", palette = 2, direction = -1) +
  scale_fill_brewer(type="qual", palette = 2, direction = -1) +
  theme_bw()

print(p)

start <- 2019
end <- 2030

gap <- function(g0, y_end = end) {
  g <- g0 * (y_end - start) / (end - start)
  total <- g * (y_end - start) / 2
  total
}


gu <- (
  filter(e, year == 2030 & scenario == 'Unconditional INDC')%>%
    mutate(m = emissions_max, emissions_max = emissions_min, emissions_min = m) %>%
    select(starts_with('emissions'))
) - (
  filter(e, year == 2030 & scenario == '<2C warming') %>%
    select(starts_with('emissions'))
)
gc <- (
  filter(e, year == 2030 & scenario == 'Conditional INDC')%>%
    mutate(m = emissions_max, emissions_max = emissions_min, emissions_min = m) %>%
    select(starts_with('emissions'))
) - (
  filter(e, year == 2030 & scenario == '<2C warming') %>%
    select(starts_with('emissions'))
)
print(map(gc, gap) %>% unlist())
print(map(gu, gap) %>% unlist())
