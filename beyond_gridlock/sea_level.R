library(dplyr)
library(ggplot2)

theme_set(theme_bw(base_size = 40))

sea_level <- data.frame(
  co2 = c(185, 280, 500, 1250),
  co2.sd = c(NA, NA, 150, 250),
  sea.level = c(-130, 0, 73 - 45, 73)
)

sea_level_2 <- data.frame(
  t = c(-5.5, 0, 1.0, 2.0, 5.0),
  t.sd = c(0.5, NA, 0.5, 0.5, 1.0),
  sea.level = c(-120, 0, 4.5, 20, 70)

)

sea_level <- sea_level %>% mutate(delta.co2 = co2 - co2[2],
                                  sea.level = sea.level * 100 / (12 * 2.54))

sea_level_2 <- sea_level_2 %>% mutate(sea.level = sea.level * 100 / (12 * 2.54))


p_1 <- ggplot(sea_level, aes(x = co2,
                      y = sea.level,
                      xmin = co2 - co2.sd,
                      xmax = co2 + co2.sd)) +
  # geom_smooth(method = "lm", fill = NA) +
  geom_line() +
  geom_errorbarh(height=20) +
  geom_point(size = 10) +
  scale_x_log10(breaks = c(100, 200, 300, 400, 500, 600, 800, 1000, 1200, 1400)) +
  scale_y_continuous(limits = c(-500,300), breaks = seq(-500,300,100), expand = c(0,0)) +
  labs(x = expression(paste(CO[2], " (parts per million)")),
       y = "Change in sea level (feet)")

annot_size <- 10

p_2 <- ggplot(sea_level_2, aes(x = t,
                             y = sea.level,
                             xmin = t - t.sd,
                             xmax = t + t.sd)) +
  # geom_smooth(method = "lm", fill = NA) +
  geom_line(data = filter(sea_level_2, t != 0)) +
  geom_errorbarh(height=20) +
  geom_point(size = 10) +
  scale_x_continuous(limits = c(-6, 8), breaks = seq(-6,10,2)) +
  scale_y_continuous(limits = c(-450,300), breaks = seq(-500,300,100), expand = c(0,0)) +
  labs(x = expression(paste("Change in global average temperature ", (degree * C))),
       y = "Change in sea level (feet)") +
  annotate("text", x = -5.9, y = -415, hjust = 0, vjust = 1, size = annot_size,
           label = "Last glacial maximum (20,000 years ago)") +
  annotate("text", x = -0.3, y = -5, hjust = 1, vjust = 1, size = annot_size,
           label = "Today") +
  annotate("text", x = 1.2, y = 40, hjust = 1, vjust = 0, size = annot_size,
           label = "Eemian (120,000 years ago)") +
  annotate("text", x = 2.1, y = 45, hjust = 0, vjust = 1, size = annot_size,
           label = "Pliocene (3 million years ago)") +
  annotate("text", x = 5, y = 250, hjust = 0.5, vjust = 0, size = annot_size,
           label = "Eocene (40 million years ago)")

p <- p_2

plot(p)

png("Sea_Level.png", height=1024, width=1024 * 1.5)
plot(p)
dev.off()
