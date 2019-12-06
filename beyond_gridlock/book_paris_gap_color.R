library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(hellno)
library(scales)
library(RColorBrewer)

theme_set(theme_bw(base_size = 60))

options(stringsAsFactors = F)

hist <- data.frame(year = c(1990,2000,2010), state = NA, level = "best",
                   emissions = c(37854, 46820, 49472), frame = "Historical")

hist2 <- data.frame(year=1990:2010, state = NA, level = "best", frame="Historical",
                    emissions = c(37853, 38305, 39589, 37720, 38463, 38656,
                                  38943, 43009, 41305, 39938, 39836, 40648,
                                  42075, 42638, 45303, 46820, 48605, 49251,
                                  48173, 48652, 49472))

fawcett <- data.frame(year = c(1990, 2005, 2010, 2030, 2050, 2100),
                     ref.no.policy = c(22.3, 29.4, 32.6, 48.3, 64.3, 94.6) * 1E3,
                     ref.low.policy = c(22.3, 29.4, 32.6, 48.3, 56.4, 69.0) * 1E3,
                     paris.continued = c(22.3, 29.4, 32.6, 39.1, 39.5, 39.4) * 1E3,
                     paris.increased = c(22.3, 29.4, 32.6, 39.1, 26.2, 6.8) * 1E3
                     )

hist2 <- hist2 %>% bind_rows(
  data.frame(year=2011:2012, state = NA, level = "best", frame="Historical",
             emissions = c(53197, 53937)) %>%
    mutate(emissions = emissions * 49472 / 50911)
)

today <- tail(hist2,1) %>% mutate(state = NA, frame = "today") %>%
  select(-level) %>%
  full_join(data.frame(frame = "today", level = c("best","min","max")), by = "frame")

indc <- bind_rows(data.frame(year = 2020, state = "unconditional",
                             level = c("best","min","max"),
                             emissions = 56602),
                  data.frame(year = 2020, state = "conditional",
                             level = c("best","min","max"),
                             emissions = 55255),
                  data.frame(year = 2025, state = "unconditional",
                             level = c("best","min","max"),
                             emissions = c(56157, 55995, 58139)),
                  data.frame(year = 2025, state = "conditional",
                             level = c("best","min","max"),
                             emissions = c(55580, 55418, 57562)),
                  data.frame(year = 2030, state = "unconditional",
                             level = c("best","min","max"),
                             emissions = c(56878, 55348, 62138)),
                  data.frame(year = 2030, state = "conditional",
                             level = c("best","min","max"),
                             emissions = c(56387, 54857, 61648)),
                  mutate(today, state = "conditional"),
                  mutate(today, state = "unconditional")) %>%
  mutate(frame = paste0("INDC (", state, ")"))

cur.pol <- bind_rows(data.frame(year = 2020, state = NA, level = c("min","max"),
                                emissions = c(53313, 57238)),
                     data.frame(year = 2025, state = NA, level = c("min","max"),
                                emissions = c(56437, 61335)),
                     data.frame(year = 2030, state = NA, level = c("min","max"),
                                emissions = c(59352, 65451)),
                     today) %>%
  mutate(frame = "Current Policy")

bau <- data.frame(year = c(2020,2025,2030), state = NA, level = "best",
                  emissions = c(59643, 63191, 66157)) %>%
  bind_rows(today) %>% mutate(frame = "Business as usual")

two.c <- bind_rows(data.frame(year = 2020,
                              level=c("best", "min", "max"),
                              emissions = c(52000, 50000, 53000)),
                   data.frame(year = 2025,
                              level=c("best", "min", "max"),
                              emissions = c(47000, 40000, 48000)),
                   data.frame(year = 2030,
                              level=c("best", "min", "max"),
                              emissions = c(42000, 30000, 44000))) %>%
  bind_rows(today) %>% mutate(frame = "Limit warming to 2C")


emissions <- bind_rows(hist2, bau, cur.pol, indc, two.c) %>% select(-state) %>%
  rename(scenario = frame) %>%
  mutate(emissions = emissions / 1000,
         level = factor(level, levels = c('best', 'min', 'max', 'best_case',
                                          'worst_case'),
                        labels = c('most.likely', 'minimum', 'maximum',
                                   'best.case', 'worst.case')),
         scenario = ordered(scenario,
                            levels = rev(c("Limit warming to 2C", "INDC (conditional)",
                                           "INDC (unconditional)", "Current Policy",
                                           "Business as usual", "Historical"))))


map_levels <- function(level, rev) {
  levels <- levels(level)
  up <- ifelse(rev, "best.case", "worst.case")
  down <- ifelse(rev, "worst.case", "best.case")
  level <- ifelse(level == "maximum", up, as.character(level))
  level <- ifelse(level == "minimum", down, level)
  factor(level, levels = levels)
}

projections <- emissions %>%
  spread(key = level, value = emissions)

pal <- brewer_pal('qual','Set1')(nlevels(emissions$scenario))

pal <- set_names(c("#806000", "#E31A1C", "#666666", "#1F78B4", "#984EA3", "#208020"),
                 rev(c('Limit warming to 2C',
                   'INDC (conditional)',
                   'INDC (unconditional)',
                   'Current Policy',
                   'Business as usual',
                   'Historical')))




trace_thickness <- 1

p <- ggplot(projections, aes(x = year, y = most.likely, ymin = minimum, ymax = maximum,
                             fill = scenario, color = scenario,
                             linetype = scenario,
                             shape = scenario, size = scenario)) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_point(size=10) +
  scale_size_manual( values = c('Limit warming to 2C' = trace_thickness,
                                'INDC (conditional)' = trace_thickness,
                                'INDC (unconditional)' = trace_thickness,
                                'Current Policy'= trace_thickness,
                                'Business as usual' = trace_thickness,
                                'Historical' = 2 * trace_thickness),
                     breaks=c('Limit warming to 2C',
                              'INDC (conditional)',
                              'INDC (unconditional)',
                              'Current Policy',
                              'Business as usual',
                              'Historical'),
                     name = "Scenario",
                     guide = guide_legend(reverse=TRUE)) +
  scale_shape_manual( values = c('Limit warming to 2C' = 16,
                                 'INDC (conditional)' = 15,
                                 'INDC (unconditional)' = 17,
                                 'Current Policy'= NA,
                                 'Business as usual' = 18,
                                 'Historical' = NA),
                      breaks=c('Limit warming to 2C',
                               'INDC (conditional)',
                               'INDC (unconditional)',
                               'Current Policy',
                               'Business as usual',
                               'Historical'),
                      name = "Scenario",
                      guide = guide_legend(reverse=TRUE)) +
  scale_linetype_manual( values = c('Limit warming to 2C' = 1,
                                    'INDC (conditional)' = 1,
                                    'INDC (unconditional)' = 1,
                                    'Current Policy'= 0,
                                    'Business as usual' = 1,
                                    'Historical' = 1),
                         breaks=c('Limit warming to 2C',
                                  'INDC (conditional)',
                                  'INDC (unconditional)',
                                  'Current Policy',
                                  'Business as usual',
                                  'Historical'),
                         name = "Scenario",
                         guide = guide_legend(reverse=TRUE)) +
  scale_fill_manual( values = pal,
                     breaks=c('Limit warming to 2C',
                              'INDC (conditional)',
                              'INDC (unconditional)',
                              'Current Policy',
                              'Business as usual',
                              'Historical'),
                     name = "Scenario",
                     guide = guide_legend(reverse=TRUE)) +
  scale_color_manual( values = pal,
                      breaks=c('Limit warming to 2C',
                               'INDC (conditional)',
                               'INDC (unconditional)',
                               'Current Policy',
                               'Business as usual',
                               'Historical'),
                      name = "Scenario",
                      guide = guide_legend(reverse=TRUE)) +
  scale_x_continuous(limits=c(NA,2025), breaks = c(seq(1990,2020,10), 2025)) +
  labs(x = "Year",
       y = expression(paste("GHG emissions (billions of tons ",  CO[2] * e / year, " )"))
       #       title = "Emissions Scenarios") +
  ) +
  #  annotate("text", x = 2024.7, y = 30.5, color = "gray", hjust = 1, vjust = 0,
  #           label = "Data Source: Den Elzen et. al., http://infographics.pbl.nl/indc/") +
  theme(legend.position = c(0.01,0.99), legend.justification=c(0,1),
        legend.text = element_text(size = 50),
        legend.key.width = unit(8, 'line'),
        legend.key.height = unit(4, 'line'))

plot(p)

png(width=1024 * 2, height=768 * 2, filename = "paris_gap_color.png")
plot(p)
dev.off()


paris_gap <- emissions %>% filter(scenario != 'Current Policy') %>%
  mutate(level = map_levels(level, scenario == "Limit warming to 2C")) %>%
  filter(! (
    (scenario %in% c("Historical", "Business as usual") & level != "most.likely") |
      (scenario == "INDC (conditional)" & level != "best.case") |
      (scenario == "INDC (unconditional)" & level != "worst.case") |
      (scenario == "Limit warming to 2C" & level == 'most.likely')
  )) %>%
  mutate(scenario = str_replace(scenario, " \\((un)?conditional\\)", ""),
         case = ifelse(level == "best.case", "best case",
                       ifelse(level == "worst.case", "worst case", "baseline")),
         level = ifelse(level == "most.likely", "trajectory",
                        ifelse(scenario == "INDC", "gap.top", "gap.bottom"))
  )


calc_gap <- function(df, which.case, max.year = NA) {
  area <- function(d) {
    a = head(d, -1)
    b = tail(d, -1)
    sum((b$year - a$year) * (b$trajectory + a$trajectory)) / 2
  }
  if (! is.na(max.year))
    df <- df %>% filter(year <= max.year)
  area(filter(df, scenario == paste0("INDC (", which.case, ")"))) -
    area(filter(df, scenario == paste0("Limit warming to 2C (", which.case, ")")))
}

pg_levels <- c(
  "Paris gap (worst case)", "Paris gap (best case)",
  "INDC (worst case)", "INDC (best case)",
  "Limit warming to 2C (worst case)",
  "Limit warming to 2C (best case)",
  "Business as usual", "Historical"
)

paris_gap <- paris_gap %>%
  mutate(scenario = ifelse(scenario %in% c("INDC", "Limit warming to 2C"),
                           str_c("Paris gap (", case, ")", sep=""), scenario)) %>%
  bind_rows(
    paris_gap %>% filter(scenario %in% c("INDC", "Limit warming to 2C")) %>%
      mutate(level = "trajectory", scenario = str_c(scenario, " (", case, ")", sep=""))
  ) %>% spread(key = level, value = emissions) %>%
  mutate(case = ordered(case, levels = c("worst case", "best case", "baseline")),
         scenario = ordered(scenario, levels = pg_levels)
  ) %>%
  arrange(scenario, case, year)

pal <- c(
  NA, NA,
  "#377EB8", "#377EB8",
  "#207E20", "#207E29",
  #  "#4DAF4A", "#4DAF4A",
  #  "#E41A1C",
  #  "#D95F02",
  "#A6761D",
  "#E7298A",
  "#802080",
  "#208020", "#208020"
)

filpal <- c(
  #  "#F0E0D0", "#F0E0D0",
  "#B0E0C0", "#B0E0C0",
  "white", "white",
  "white",
  "white",
  "white", "white",
  "white", "white"
)

bks <- c("Historical", "Business as usual",
         "INDC (worst case)", "INDC (best case)",
         "Limit warming to 2C (worst case)", "Limit warming to 2C (best case)",
         "Paris gap (worst case)", "Paris gap (best case)"
)

names(pal) <- pg_levels
names(filpal) <- pg_levels

pg <- paris_gap %>% filter(case != "best case")
bk <- bks %>% keep(~.x %in% unique(pg$scenario))

emissions.gap <- calc_gap(paris_gap, "worst case", 2025)
gap_label <- paste0('paste("Worst case Paris gap = ", ',
                    formatC(emissions.gap, digits=0, format='f'),
                    ' ~ GT ~ CO[2] * e, " through 2025")')

print(gap_label)

p2 <- ggplot(pg,
             aes(x = year, y = trajectory, ymin = gap.bottom, ymax = gap.top,
                 color = scenario, shape = scenario, fill = scenario,
                 linetype = scenario, size = scenario)) +
  geom_ribbon() +
  geom_line() +
  geom_point(size = 10, fill = "black") +
  scale_size_manual( values = c('Limit warming to 2C (worst case)' = trace_thickness,
                                'Limit warming to 2C (best case)' = trace_thickness,
                                'INDC (worst case)' = trace_thickness,
                                'INDC (best case)' = trace_thickness,
                                'Paris gap (worst case)' = trace_thickness,
                                'Paris gap (best case)' = trace_thickness,
                                'Business as usual' = trace_thickness,
                                'Historical' = 2 * trace_thickness),
                     breaks=c('Limit warming to 2C (worst case)',
                              'Limit warming to 2C (best case)',
                              'INDC (worst case)',
                              'INDC (best case)',
                              'Paris gap (worst case)',
                              'Paris gap (best case)',
                              'Business as usual',
                              'Historical'),
                     guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_shape_manual( values = c('Limit warming to 2C (worst case)' = 16,
                                 'Limit warming to 2C (best case)' = 16,
                                 'INDC (worst case)' = 15,
                                 'INDC (best case)' = 15,
                                 'Paris gap (worst case)' = NA,
                                 'Paris gap (best case)' = NA,
                                 'Business as usual' = 17,
                                 'Historical' = NA),
                      breaks=c('Limit warming to 2C (worst case)',
                               'Limit warming to 2C (best case)',
                               'INDC (worst case)',
                               'INDC (best case)',
                               'Paris gap (worst case)',
                               'Paris gap (best case)',
                               'Business as usual',
                               'Historical'),
                      guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_linetype_manual( values = c('Limit warming to 2C (worst case)' = 1,
                                    'Limit warming to 2C (best case)' = 1,
                                    'INDC (worst case)' = 1,
                                    'INDC (best case)' = 1,
                                    'Paris gap (worst case)' = 0,
                                    'Paris gap (best case)' = 0,
                                    'Business as usual' = 1,
                                    'Historical' = 1),
                         breaks=c('Limit warming to 2C (worst case)',
                                  'Limit warming to 2C (best case)',
                                  'INDC (worst case)',
                                  'INDC (best case)',
                                  'Paris gap (worst case)',
                                  'Paris gap (best case)',
                                  'Business as usual',
                                  'Historical'),
                         guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_fill_manual( values = c('Limit warming to 2C (worst case)' = NA,
                                'Limit warming to 2C (best case)' = NA,
                                'INDC (worst case)' = NA,
                                'INDC (best case)' = NA,
                                'Paris gap (worst case)' = alpha("black", 0.3),
                                'Paris gap (best case)' = alpha("black", 0.3),
                                'Business as usual' = NA,
                                'Historical' = NA),
                     breaks=c('Limit warming to 2C (worst case)',
                              'Limit warming to 2C (best case)',
                              'INDC (worst case)',
                              'INDC (best case)',
                              'Paris gap (worst case)',
                              'Paris gap (best case)',
                              'Business as usual',
                              'Historical'),
                     guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_color_manual( values = c('Limit warming to 2C (worst case)' = "black",
                                 'Limit warming to 2C (best case)' = "black",
                                 'INDC (worst case)' = "black",
                                 'INDC (best case)' = "black",
                                 'Paris gap (worst case)' = "black",
                                 'Paris gap (best case)' = "black",
                                 'Business as usual' = "black",
                                 'Historical' = "black"),
                      breaks=c('Limit warming to 2C (worst case)',
                               'Limit warming to 2C (best case)',
                               'INDC (worst case)',
                               'INDC (best case)',
                               'Paris gap (worst case)',
                               'Paris gap (best case)',
                               'Business as usual',
                               'Historical'),
                      guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_y_continuous(limits = c(29,70), expand = c(0,0)) +
  labs(x = "Year",
       y = expression(paste("GHG emissions (billions of tons ",  CO[2] * e / year, " )"))
       #       title = "Emissions Scenarios (Worst Case)"
  ) +
  #  annotate("text", x = 2024.7, y = 30.5, color = "gray", hjust = 1, vjust = 0,
  #           label = "Data Source: Den Elzen et. al., http://infographics.pbl.nl/indc/") +
  #  annotate("label", x = 2010, y = 35, fill = "white", color = "gray20",
  #           label.padding = unit(0.4, "lines"),
  #           label = gap_label, parse=TRUE,
  #           size = 10) +
  theme(legend.position = c(0.01,0.99), legend.justification=c(0,1),
        legend.text = element_text(size = 50),
        legend.key.width = unit(8, 'lines'),
        legend.key.height = unit(4, 'lines'))

plot(p2)

png(width=1024 * 2, height=768 * 2, filename = "paris_gap_worst_case_color.png")
plot(p2)
dev.off()

pg2 <- paris_gap %>% filter(case != "worst case")
bk <- bks %>% keep(~.x %in% unique(pg2$scenario))

emissions.gap <- calc_gap(paris_gap, "best case", 2025)
gap_label <- paste0('paste("Best case Paris gap = ", ',
                    formatC(emissions.gap, digits=0, format='f'),
                    ' ~ GT ~ CO[2] * e, " through 2025")')

print(gap_label)

p3 <- ggplot(pg2,
             aes(x = year, y = trajectory, ymin = gap.bottom, ymax = gap.top,
                 color = scenario, shape = scenario, fill = scenario,
                 linetype = scenario, size = scenario)) +
  geom_ribbon() +
  geom_line() +
  geom_point(size = 10, fill = "black") +
  scale_size_manual( values = c('Limit warming to 2C (worst case)' = trace_thickness,
                                'Limit warming to 2C (best case)' = trace_thickness,
                                'INDC (worst case)' = trace_thickness,
                                'INDC (best case)' = trace_thickness,
                                'Paris gap (worst case)' = trace_thickness,
                                'Paris gap (best case)' = trace_thickness,
                                'Business as usual' = trace_thickness,
                                'Historical' = 2 * trace_thickness),
                     breaks=c('Limit warming to 2C (worst case)',
                              'Limit warming to 2C (best case)',
                              'INDC (worst case)',
                              'INDC (best case)',
                              'Paris gap (worst case)',
                              'Paris gap (best case)',
                              'Business as usual',
                              'Historical'),
                     guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_shape_manual( values = c('Limit warming to 2C (worst case)' = 16,
                                 'Limit warming to 2C (best case)' = 16,
                                 'INDC (worst case)' = 15,
                                 'INDC (best case)' = 15,
                                 'Paris gap (worst case)' = NA,
                                 'Paris gap (best case)' = NA,
                                 'Business as usual' = 17,
                                 'Historical' = NA),
                      breaks=c('Limit warming to 2C (worst case)',
                               'Limit warming to 2C (best case)',
                               'INDC (worst case)',
                               'INDC (best case)',
                               'Paris gap (worst case)',
                               'Paris gap (best case)',
                               'Business as usual',
                               'Historical'),
                      guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_linetype_manual( values = c('Limit warming to 2C (worst case)' = 1,
                                    'Limit warming to 2C (best case)' = 1,
                                    'INDC (worst case)' = 1,
                                    'INDC (best case)' = 1,
                                    'Paris gap (worst case)' = 0,
                                    'Paris gap (best case)' = 0,
                                    'Business as usual' = 1,
                                    'Historical' = 1),
                         breaks=c('Limit warming to 2C (worst case)',
                                  'Limit warming to 2C (best case)',
                                  'INDC (worst case)',
                                  'INDC (best case)',
                                  'Paris gap (worst case)',
                                  'Paris gap (best case)',
                                  'Business as usual',
                                  'Historical'),
                         guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_fill_manual( values = c('Limit warming to 2C (worst case)' = NA,
                                'Limit warming to 2C (best case)' = NA,
                                'INDC (worst case)' = NA,
                                'INDC (best case)' = NA,
                                'Paris gap (worst case)' = alpha("black", 0.3),
                                'Paris gap (best case)' = alpha("black", 0.3),
                                'Business as usual' = NA,
                                'Historical' = NA),
                     breaks=c('Limit warming to 2C (worst case)',
                              'Limit warming to 2C (best case)',
                              'INDC (worst case)',
                              'INDC (best case)',
                              'Paris gap (worst case)',
                              'Paris gap (best case)',
                              'Business as usual',
                              'Historical'),
                     guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_color_manual( values = c('Limit warming to 2C (worst case)' = "black",
                                 'Limit warming to 2C (best case)' = "black",
                                 'INDC (worst case)' = "black",
                                 'INDC (best case)' = "black",
                                 'Paris gap (worst case)' = "black",
                                 'Paris gap (best case)' = "black",
                                 'Business as usual' = "black",
                                 'Historical' = "black"),
                      breaks=c('Limit warming to 2C (worst case)',
                               'Limit warming to 2C (best case)',
                               'INDC (worst case)',
                               'INDC (best case)',
                               'Paris gap (worst case)',
                               'Paris gap (best case)',
                               'Business as usual',
                               'Historical'),
                      guide = guide_legend(reverse=TRUE), drop = TRUE) +
  scale_y_continuous(limits = c(29,70), expand = c(0,0)) +
  labs(x = "Year",
       y = expression(paste("GHG emissions (billions of tons ",  CO[2] * e / year, " )"))
       # title = "Emissions Scenarios (Best Case)"
  ) +
  #  annotate("text", x = 2024.7, y = 30.5, color = "gray", hjust = 1, vjust = 0,
  #           label = "Data Source: Den Elzen et. al., http://infographics.pbl.nl/indc/") +
  #  annotate("label", x = 2010, y = 35, fill = "white", color = "gray20",
  #           label.padding = unit(0.4, "lines"),
  #           label = gap_label, parse=TRUE,
  #           size = 10) +
  theme(legend.position = c(0.01,0.99), legend.justification=c(0,1),
        legend.text = element_text(size = 50),
        legend.key.width = unit(8, 'lines'),
        legend.key.height = unit(4, 'lines'))

plot(p3)

png(width=1024 * 2, height=768 * 2, filename = "paris_gap_best_case_color.png")
plot(p3)
dev.off()

fawcett_gap <- function(fawcett, rcp_scenarios) {
  area <- function(df) {
    a = head(df, -1)
    b = tail(df, -1)
    sum((b$year - a$year) * (b$trajectory + a$trajectory)) / 2
  }
  f <- fawcett %>% gather(-year, key = scenario, value = emissions)
  g <- rcp_scenarios %>% filter(Variable == "Emissions|CO2|Fossil fuels and Industry" & Scenario %in% c('GCAM2.6','RCP4.5')) %>%
    select(region = Region, year = Year, emissions = value, scenario = Scenario) %>%
    mutate(scenario = scenario %>% str_to_lower() %>% str_replace_all(fixed('.'),'')) %>%
    group_by(year, scenario) %>%
    summarize(emissions = sum(emissions)) %>%
    ungroup()
  emissions <- bind_rows(f,g)
  x <- data.frame(year = 2005:2030)
  for (s in emissions$scenario) {
    xx <- emissions %>% filter(scenario == s)
    yy <- approx(xx$year, xx$emissions, x$year)$y
    x <- x %>% mutate_(.dots = setNames(list(~yy), list(s)))
  }
  x <- x %>% filter(year > 2015 & year <= 2025)
  gap26 <- x %>% mutate(trajectory = paris.continued - gcam26) %>% area()
  gap45 <- x %>% mutate(trajectory = paris.continued - rcp45) %>% area()
  c(gap_2c = gap26, gap_3c = gap45)
}
