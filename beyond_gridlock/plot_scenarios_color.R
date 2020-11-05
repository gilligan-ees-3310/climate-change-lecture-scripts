library(RColorBrewer)
library(ggplot2)
library(grid)

source('read_gcam_scenarios.R')

palette <- RColorBrewer::brewer.pal(8, "Dark2")[-(5:6)]
names(palette) <- levels(global_emissions$Scenario)

start_year <- 1800
axis_start_year <- 1800
delta_year <- 50

theme_jg <- theme_bw(base_size =12)

trace_thickness <- 1

ge <- global_emissions %>% filter(! Scenario %in% c('Business As Usual'))

pal <- scales::brewer_pal('qual','Dark2')(nlevels(ge$Scenario))

pal = palette[-5]

# pal <- c("#806000", "#E31A1C", "#666666", "#1F78B4", "#984EA3", "#208020")


p1.5 <- ggplot(ge,
               aes(x=Year, y=emissions,
                   color=Scenario, group=Scenario, size = Scenario,
                   shape=Scenario #, linetype=Scenario, size=Scenario
                   )) +
  # geom_hline(yintercept = 0, color='dark gray') +
  geom_hline(yintercept = 0, color='dark gray') +
  geom_line() +
  geom_point(size=2) +
  # scale_linetype_manual( values = c('RCP 2.6' = 2, 'RCP 4.5' = 2, 'RCP 6.0' = 2,
  #                                   'RCP 8.5'= 2,'Business As Usual' = 2,
  #                                   'Historical' = 1,
  #                                   's450' = 5, 'newpolicy' = 5, 'bau' = 5),
  #                        breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
  #                                 'Business As Usual', 'Historical',
  #                                 's450', 'newpolicy', 'bau'),
  #                        labels = c(warming_labels,
  #                                   newpolicy= 'IEA new policy',
  #                                   bau = 'IEA Business as Usual',
  #                                   s450 = 'IEA 450 ppm'),
  #                        guide = guide_legend(reverse=TRUE)) +
  scale_size_manual( values = c('RCP 2.6' = trace_thickness, 'RCP 4.5' = trace_thickness, 'RCP 6.0' = trace_thickness,
                                    'RCP 8.5'= trace_thickness,'Business As Usual' = trace_thickness,
                                    'Historical' = 2),
                         breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
                                  'Business As Usual', 'Historical'),
                         labels = c(warming_labels),
                         guide = guide_legend(reverse=TRUE)) +
  scale_color_manual(
    values = palette,
    breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
             'Business As Usual', 'Historical'),
    labels = c(warming_labels),
    guide=guide_legend(reverse=TRUE)) +
  scale_shape_manual( values = c('RCP 2.6' = 16, 'RCP 4.5' = 15, 'RCP 6.0' = 17,
                                 'RCP 8.5'= 18,'Business As Usual' = 3,
                                 'Historical' = NA),
                      breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
                               'Business As Usual', 'Historical'),
                      labels = c(warming_labels),
                      guide = guide_legend(reverse=TRUE)) +
  # scale_x_continuous(limits = c(1800,2100), breaks = seq(1800,2100,50)) +
  scale_x_continuous(limits = c(axis_start_year, 2100), breaks=seq(axis_start_year,2100,delta_year)) +
  labs(x = "Year", y = expression(paste(CO[2], " emissions (billion tons/year)"))) +
  theme_jg + theme(legend.position = c(0.05,0.95), legend.justification = c(0,1),
                   legend.key.width = unit(3, "line"))

print(p1.5)

start_year <- 1800
axis_start_year <- 1800
delta_year <- 25


p1.4 <- ggplot(global_emissions %>% filter(Scenario == 'Business As Usual'), aes(x=Year, y=emissions, color=Scenario, group=Scenario)) +
  geom_hline(yintercept = 0, color='black') +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_line(size=1, data = global_emissions %>% filter(Scenario == 'Historical' & Year >= start_year)) +
  scale_x_continuous(breaks=seq(axis_start_year,2100,delta_year)) +
  scale_color_manual(values=palette,
                     breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5', 'Business As Usual', 'Historical'),
                     labels = warming_labels,
                     guide=guide_legend(reverse=TRUE)) +
  labs(x = "Year", y = expression(paste(CO[2], " emissions (billion tons/year)"))) +
  theme_jg

print(p1.4)

p1.6 <- ggplot(global_emissions %>% filter(! Scenario %in% c('Historical'),
                                         Year %in% 1990:2040),
             aes(x=Year, y=emissions, color=Scenario, group=Scenario)) +
  geom_hline(yintercept = 0, color='black') +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_line(size=1, data = global_emissions %>% filter(Scenario == 'Historical' & Year >= start_year)) +
  scale_x_continuous(limits = c(1990,2040), breaks=seq(1990,2100,15)) +
  scale_color_manual(values=palette,
                     breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5', 'Business As Usual', 'Historical'),
                     labels = warming_labels,
                     guide=guide_legend(reverse=TRUE)) +
  labs(x = "Year", y = expression(paste(CO[2], " emissions (billion tons/year)"))) +
  theme_jg

print(p1.6)

upper_year <- 2040

p1.7 <- ggplot(global_emissions %>% filter(! Scenario %in% c('Historical'),
                                         Year %in% 1990:upper_year),
             aes(x=Year, y=emissions, color=Scenario, group=Scenario)) +
  geom_hline(yintercept = 0, color='black') +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_line(size=1, data = global_emissions %>% filter(Scenario == 'Historical' & Year >= start_year)) +
  scale_x_continuous(limits = c(1990,upper_year), breaks=seq(1990,2100,15)) +
  scale_color_manual(values=palette,
                     breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5', 'Business As Usual', 'Historical'),
                     labels = warming_labels,
                     guide=guide_legend(reverse=TRUE)) +
  labs(x = "Year", y = expression(paste(CO[2], " emissions (billion tons/year)"))) +
  theme_jg

print(p1.7)

upper_year <- 2040


p1.8 <- ggplot(newdata  %>% filter(! Scenario %in% c('Historical'),
                                 Year %in% 1990:upper_year),
             aes(x=Year, y=emissions, color=Scenario,
                 group=Scenario,
                 shape=Scenario,
                 linetype = Scenario)) +
  geom_hline(yintercept = 0, color='dark gray') +
  geom_line(size=1) +
  geom_point(size=3, color = "black") +
  geom_line(size=1, data = newdata %>%
              filter(Scenario == 'Historical' & Year >= start_year)) +
  scale_x_continuous(limits = c(1990,upper_year), breaks=seq(1990,2100,15)) +
  scale_linetype_manual( values = c('RCP 2.6' = 4, 'RCP 4.5' = 4, 'RCP 6.0' = 4,
                                    'RCP 8.5'= 4,'Business As Usual' = 4,
                                    'Historical' = 1,
                                    's450' = 5, 'newpolicy' = 5, 'bau' = 5),
                         breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
                                  'Business As Usual', 'Historical',
                                  's450', 'newpolicy', 'bau'),
                         labels = c(warming_labels,
                                    newpolicy= 'IEA new policy',
                                    bau = 'IEA Business as Usual',
                                    s450 = 'IEA 450 ppm'),
                         guide = guide_legend(reverse=TRUE)) +
  scale_color_manual( # values = rev(c(brewer.pal(3,'Set1'), brewer.pal(8,'Dark2')[-(5:6)])),
                      values = c('RCP 2.6' = "gray30", 'RCP 4.5' = "gray30", 'RCP 6.0' = "gray30",
                                 'RCP 8.5'= "gray30",'Business As Usual' = "gray30",
                                 'Historical' = "black",
                                 's450' = 5, 'newpolicy' = 5, 'bau' = 5),
                      breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
                                 'Business As Usual', 'Historical',
                                 's450', 'newpolicy', 'bau'),
                        labels = c(warming_labels,
                                   newpolicy= 'IEA new policy',
                                   bau = 'IEA Business as Usual',
                                   s450 = 'IEA 450 ppm'),
                        guide=guide_legend(reverse=TRUE)) +
  scale_shape_manual( values = c('RCP 2.6' = 16, 'RCP 4.5' = 15, 'RCP 6.0' = 17,
                                 'RCP 8.5'= 18,'Business As Usual' = 3,
                                 'Historical' = 1,
                                 's450' = 2, 'newpolicy' = 5, 'bau' = 6),
                      breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
                               'Business As Usual', 'Historical',
                               's450', 'newpolicy', 'bau'),
                      labels = c(warming_labels,
                                 newpolicy= 'IEA new policy',
                                 bau = 'IEA Business as Usual',
                                 s450 = 'IEA 450 ppm'),
                      guide = guide_legend(reverse=TRUE)) +
  labs(x = "Year", y = expression(paste(CO[2], " emissions (billion tons/year)"))) +
  theme_jg + theme(legend.key.width = unit(0.05, 'npc'))

print(p1.8)

p1.9 <-
  carbon_price %>%
  mutate(Scenario = fct_recode(Scenario, "RCP 2.6" = "GCAM2.6",
                               "RCP 4.5" = "RCP4.5", "RCP 6.0" = "GCAM6.0",
                               "RCP 8.5" = "GCAM8.5",
                               "Business As Usual" = "GCAMReference")) %>%
  ggplot(aes(x = Year, y = mean_price, color = Scenario, group = Scenario, shape = Scenario)) +
  geom_line(size=1) + geom_point(size=3) +
  scale_x_continuous(breaks = seq(1990,2100,15)) +
  scale_color_manual(
    values = palette,
    breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
             'Business As Usual', 'Historical'),
    labels = c(warming_labels),
    guide=guide_legend(reverse=FALSE)) +
  scale_shape_manual( values = c('RCP 2.6' = 16, 'RCP 4.5' = 15, 'RCP 6.0' = 17,
                                 'RCP 8.5'= 18,'Business As Usual' = 3,
                                 'Historical' = 1,
                                 's450' = 2, 'newpolicy' = 5, 'bau' = 6),
                      breaks=c('RCP 2.6', 'RCP 4.5', 'RCP 6.0', 'RCP 8.5',
                               'Business As Usual', 'Historical',
                               's450', 'newpolicy', 'bau'),
                      labels = c(warming_labels,
                                 newpolicy= 'IEA new policy',
                                 bau = 'IEA Business as Usual',
                                 s450 = 'IEA 450 ppm'),
                      guide = guide_legend(reverse=FALSE)) +
  scale_y_continuous(breaks = seq(0, 1000, 100)) +
  labs(x = "Year", y =  expression(paste("Carbon Price (2005 dollars/ton ", CO[2], ")"))) +
  theme_jg +
  theme(legend.position = c(0.70, 0.87), legend.justification = c(1,1))

print(p1.9)

png(filename = 'fig_1_4.png', width = 6.5, height=3.8, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p1.4)
dev.off()

png(filename = 'fig_1_5.png', width = 6.5, height=3.8, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p1.5)
dev.off()


png(filename = 'fig_1_6.png', width = 6.5, height=3.8, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p1.6)
dev.off()


png(filename = 'fig_1_7.png', width = 6.5, height=3.8, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p1.7)
dev.off()


png(filename = 'fig_1_8.png', width = 6.5, height=3.8, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p1.8)
dev.off()


png(filename = 'fig_1_9.png', width = 6.5, height=3.8, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p1.9)
dev.off()

