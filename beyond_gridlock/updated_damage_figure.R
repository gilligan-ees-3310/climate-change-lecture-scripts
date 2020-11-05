#
# New fiture based on the IPCC WG2 table for figure 10-17 Figure 10.
#

library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)

theme_jg <- theme_bw(base_size =12)
data_dir <- 'data'

data <- read.csv(file.path(data_dir, 'ipcc_wg2_table_sm10_2.csv'), header=TRUE, stringsAsFactors = F)
names(data) <- c('study','warming','damage','method','notes')
data$notes <- str_trim(data$notes)

p <- ggplot(data, aes(x = warming, y = damage)) +
  geom_point(size = 5, alpha = 0.5, color = "dark blue") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(0,6), breaks = 0:6) +
  scale_y_continuous(limits = c(-15,5), breaks = seq(-15,5,5),
                     minor_breaks = seq(-15,5,1)) +
  labs(x = expression(paste("Warming ", (degree * C))),
       y = "Change in global income (% GDP)") +
  theme_jg

print(p)


png(filename = 'fig_1_3.png', width = 6.5, height=3.0, units="in", res=300,
    pointsize=10, type = 'cairo',
    family = 'serif')
print(p)
dev.off()
