#
#  Read flask data and plot CO2 vs. delta 13C
#

read.13 <- function(filename) {
  data <- read.table(file.path(data_dir, 'keeling', filename), header=F,
                     col.names=c("site","year", "month", "delta"), skip=70)
  data$Frac.Year <- data$year + (data$month - 0.5)/12
  df <- data.frame(year = data$Frac.Year, d13 = data$delta)
  return(df)
}

read.14 <- function(filename) {
  data <- read.table(file.path(data_dir, 'keeling', filename), header=F,
                     col.names=c("site","year", "month", "delta"), skip=38)
  data$Frac.Year <- data$year + (data$month - 0.5)/12
  df <- data.frame(year = data$Frac.Year, d14 = data$delta)
  return(df)
}

read.co2 <- function(filename) {
  data <- read.table(file.path(data_dir, 'keeling', filename), header=F,
                     col.names=c("site","year", "month", "co2"), skip=69)
  data$Frac.Year <- data$year + (data$month - 0.5)/12
  df <- data.frame(year = data$Frac.Year, co2 = data$co2)
  return(df)
}

read.scripps <- function(filename, varname) {
  data <- read.table(file.path(data_dir, 'keeling', filename), header=F,
                     col.names = c("year", "raw", "norm", "rsmooth", "nsmooth",
                                   "sample.date"))
  df <- data[,c('year','rsmooth')]
  colnames(df) <- c('year',varname)
  return(df)
}


plot.nwr <- function() {
  d13 <- read.13('co2c13_nwr_surface-flask_1_sil_month.txt')
  d14 <- read.14('co2c14_flask_nwr_all_month.txt')
  co2 <- read.co2('co2_nwr_surface-flask_1_ccgg_month.txt')
  co2$var <- 'CO2'
  d13$var <- 'delta.13'
  d14$var <- 'delta.14'
  names(d13) <- c('year','val','var')
  names(d14) <- c('year','val','var')
  names(co2) <- c('year','val','var')
  df <- rbind(co2,d14,d13)
  df$var <- factor(df$var, levels=c('CO2','delta.13','delta.14'),ordered=TRUE)
  annot <- data.frame(x=2003.7, y=c(382, -8.33, 58),
                      label=c('CO[2] * " " * ("ppm")',
                              'delta^13 * C * " " * ("\u2030")',
                              'delta^14 * C * " " * ("\u2030")'),
                      var=c('CO2','delta.13','delta.14'))

  p <- ggplot(df[df$year >= 2003.375 & df$year <= 2007.125,],
              aes(x=year,y=val, color=var)) +
    geom_line(size=I(1)) +
    geom_text(aes(x=x,y=y,label=label), data=annot, parse=TRUE, size=8) +
    facet_grid(var~., scales='free') +
    scale_color_brewer(type="qual", palette="Dark2") +
    theme_bw(base_size=20) +
    theme(legend.position='none', strip.text=element_blank(),
          strip.background=element_blank(), axis.title.y=element_blank())
  return(p)
}

plot.scripps <- function(xmin = 1995, xmax = NA, note_x = xmin + 2.5,
                         note_delta_y = c(0, 0, 0)) {
  d13 <- read.13('co2c13_mlo_surface-flask_1_sil_month.txt')
  o2 <- read.scripps('mloo.txt','o2')
  co2 <- read.scripps('mloc.txt','co2')
  co2$var <- 'CO2'
  d13$var <- 'delta.13'
  o2$var <- 'O2'
  names(d13) <- c('year','val','var')
  names(o2) <- c('year','val','var')
  names(co2) <- c('year','val','var')
  df <- rbind(co2,o2,d13)
  df$var <- factor(df$var, levels=c('CO2','O2','delta.13'),ordered=TRUE)
  annot <- data.frame(x=note_x, y=c(385, -450, -8.475) + note_delta_y,
                      label=c('CO[2] * " " * ("ppm")',
                              'delta * O[2] * " " * ("meg")',
                              'delta^13 * C * " " * ("\u2030")'),
                      var=c('CO2','O2','delta.13'))

  p <- ggplot(df,
              aes(x=year,y=val, color=var)) +
    geom_line(size=I(1)) +
    geom_text(aes(x=x,y=y,label=label), data=annot, parse=TRUE, size=8) +
    xlim(xmin, xmax) +
    facet_grid(var~., scales='free') +
    scale_color_brewer(type="qual", palette="Dark2") +
    theme_bw(base_size=20) +
    theme(legend.position='none', strip.text=element_blank(),
          strip.background=element_blank(), axis.title.y=element_blank())
  return(p)
}
