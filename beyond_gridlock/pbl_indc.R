library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(hellno)
library(scales)
library(RColorBrewer)

options(stringsAsFactors = F)

hist <- data.frame(year = c(1990,2000,2010), state = NA, level = "best",
                   emissions = c(37854, 46820, 49472), frame = "Historical")

hist2 <- data.frame(year=1990:2010, state = NA, level = "best", frame="Historical",
                    emissions = c(37853, 38305, 39589, 37720, 38463, 38656,
                                  38943, 43009, 41305, 39938, 39836, 40648,
                                  42075, 42638, 45303, 46820, 48605, 49251,
                                  48173, 48652, 49472))

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

pal <- c("#806000", "#E31A1C", "#666666", "#1F78B4", "#984EA3", "#208020")

p <- ggplot(projections, aes(x = year, y = most.likely, ymin = minimum, ymax = maximum,
                             color = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.4) +
  geom_line(size=1) +
  geom_point(size=3) +
  scale_color_manual(values=pal, name = "Scenario") +
  scale_fill_manual(values=pal, name = "Scenario") +
  scale_x_continuous(limits=c(NA,2025), breaks = c(seq(1990,2020,10), 2025)) +
  labs(x = "Year", y = expression(paste("Emissions (billions of tons ",  CO[2] * e / year, " )")),
       title = "Emissions Scenarios") +
  annotate("text", x = 2024.7, y = 30.5, color = "gray", hjust = 1, vjust = 0,
           label = "Data Source: Den Elzen et. al., http://infographics.pbl.nl/indc/") +
  theme_bw(base_size = 20) +
  theme(legend.position = c(0.01,0.99), legend.justification=c(0,1))

plot(p)


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
  "#E41A1C",
#  "#D95F02",
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

p2 <- ggplot(pg,
aes(x = year, y = trajectory, ymin = gap.bottom, ymax = gap.top, color = scenario)) +
  geom_ribbon(alpha = 0.9, aes(fill = scenario)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values=pal, name = "Scenario", drop=TRUE, breaks = bk) +
  scale_fill_manual(values = filpal, name = "Scenario", drop=TRUE, breaks = bk) +
  scale_x_continuous(limits=c(NA,2025), breaks = c(seq(1990,2020,10), 2025)) +
  labs(x = "Year", y = expression(paste("Emissions (billions of tons ",  CO[2] * e / year, " )")),
       title = "Emissions Scenarios (Worst Case)") +
  annotate("text", x = 2024.7, y = 30.5, color = "gray", hjust = 1, vjust = 0,
           label = "Data Source: Den Elzen et. al., http://infographics.pbl.nl/indc/") +
  annotate("label", x = 2010, y = 35, fill = "white", color = "gray20",
           label.padding = unit(0.4, "lines"),
           label = gap_label, parse=TRUE,
           size = 10) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(0.01,0.99), legend.justification=c(0,1))

plot(p2)

png(width=1024 * 2, height=768 * 2, filename = "paris_gap_worst_case.png")
plot(p2)
dev.off()

pg2 <- paris_gap %>% filter(case != "worst case")
bk <- bks %>% keep(~.x %in% unique(pg2$scenario))

emissions.gap <- calc_gap(paris_gap, "best case", 2025)
gap_label <- paste0('paste("Best case Paris gap = ", ',
                    formatC(emissions.gap, digits=0, format='f'),
                    ' ~ GT ~ CO[2] * e, " through 2025")')


p3 <- ggplot(pg2,
             aes(x = year, y = trajectory, ymin = gap.bottom, ymax = gap.top, color = scenario)) +
  geom_ribbon(alpha = 0.9, aes(fill = scenario)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  scale_color_manual(values=pal, name = "Scenario", drop=TRUE, breaks = bk) +
  scale_fill_manual(values = filpal, name = "Scenario", drop=TRUE, breaks = bk) +
  scale_x_continuous(limits=c(NA,2025), breaks = c(seq(1990,2020,10), 2025)) +
  labs(x = "Year", y = expression(paste("Emissions (billions of tons ",  CO[2] * e / year, " )")),
       title = "Emissions Scenarios (Best Case)") +
  annotate("text", x = 2024.7, y = 30.5, color = "gray", hjust = 1, vjust = 0,
           label = "Data Source: Den Elzen et. al., http://infographics.pbl.nl/indc/") +
  annotate("label", x = 2010, y = 35, fill = "white", color = "gray20",
           label.padding = unit(0.4, "lines"),
           label = gap_label, parse=TRUE,
           size = 10) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(0.01,0.99), legend.justification=c(0,1))

plot(p3)

png(width=1024 * 2, height=768 * 2, filename = "paris_gap_best_case.png")
plot(p3)
dev.off()


