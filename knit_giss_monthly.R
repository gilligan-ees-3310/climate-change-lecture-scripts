#
# GISS monthly
#
library(tidyverse)

library(ggthemes)
library(grid)

extract_years <- function(giss_data, dec.nov = FALSE) {
  if (dec.nov) {
    target_month <- 'may'
  } else {
    target_month <- 'jun'
  }
  annual_data <- giss_data %>% filter(month == target_month)
  invisible(annual_data)
}

warmest_years <- function(annual_data, n = 14) {
  annual_data %>% top_n(n, t.anom.annual) %>% arrange(desc(t.anom.annual)) %>%
    invisible()
}

make_baseline <- function(annual_data, start, end) {
  annual_data <- annual_data %>% filter(year >= start & year <= end)
  baseline <- data.frame(time = c(start, end + 11./12),
                         t.anom.annual = mean(annual_data$t.anom.annual))
  invisible(baseline)
}

make_gisplot <- function(data,
                         n_warmest = 14,
                         inc_last_month = FALSE,
                         baseline_years = c(1951,1980),
                         dec.nov = FALSE,
                         warm_row = c(4,6),
                         warm_row_pos = 0.12,
                         base_size = 30) {
  data.source.text <- data$label
  monthly_data <- data$data
  bl <- (monthly_data %>% filter(year >= min(baseline_years) & year <= max(baseline_years)) %>%
    summarize(baseline = mean(t.anom, na.rm=T)))$baseline[[1]]
  monthly_data <- monthly_data %>%
    mutate(t.anom = t.anom - bl, t.anom.annual = t.anom.annual - bl,
           t.anom.decadal = t.anom.decadal - bl)
  annual_data <- extract_years(monthly_data, dec.nov) %>% filter(! is.na(t.anom.annual))
  warmest <- warmest_years(annual_data, n_warmest)
  baseline <- make_baseline(annual_data, baseline_years[1], baseline_years[2])

  min_year <- min(annual_data$year);   max_year <- max(annual_data$year)
  last_year <- tail(annual_data,1)
  last_month <- tail(filter(monthly_data, ! is.na(t.anom)), 1)
  min_temp <- min(monthly_data$t.anom.annual, na.rm=T)
  max_temp <- max(monthly_data$t.anom.annual, na.rm=T)
  if (inc_last_month) {
    min_temp <- min(min_temp, last_month$t.anom)
    max_temp <- max(max_temp, last_month$t.anom)
  }

  if (dec.nov) {
    year_spec <- 'Dec.-Nov.'
  } else {
    year_spec <- 'Jan.-Dec.'
  }

  warmtitle = paste0(n_warmest, " warmest years (", year_spec, ")")
  basetitle = paste0("Baseline (", baseline_years[1], "-", baseline_years[2], ")")
  last_pt <- substitute(paste( year, "@ ", temp*degree*C),
                        list(year=last_year$year,
                             temp=formatC(last_year$t.anom.annual, digits=2,
                                          drop0trailing = FALSE, format='f')))
  last_m_pt <- substitute(paste( mon, year, "@ ", temp*degree*C),
                          list(mon = paste0(str_to_title(as.character(last_month$month),'.')),
                               year=last_month$year,
                               temp=formatC(last_month$t.anom, digits = 2,
                                            drop0trailing = FALSE, format='f')))

  gisplot <- ggplot(monthly_data, aes(time, t.anom.annual))
  gisplot <- gisplot + geom_point(data = annual_data, aes(color="yearly"),size=I(4))
  gisplot <- gisplot + geom_line(aes(color="yearly"),size=I(1))
  gisplot <- gisplot + geom_line(aes(y=t.anom.decadal,color="decadal"), size=I(1.5))
  gisplot <- gisplot + geom_point(data=last_year, aes(color="last"), size=I(7))
  gisplot <- gisplot + geom_point(data=warmest, aes(color="hottest"), size=I(4))
  if (inc_last_month) {
    gisplot <- gisplot + geom_point(data=last_month, aes(y = t.anom,
                                                         color="last.month"), size=I(4))
  }
  gisplot <- gisplot + geom_line(data=baseline, aes(color="baseline"), linetype=2,size=I(1))
  if (inc_last_month) {
    aes_list <- list(shape=c(16,NA,NA,16,16,16),
                     linetype=c(0,1,2,0,0,0),
                     size=c(4,2,1,4,4,4),
                     lwd=c(1,2,1,1,1,1))
    scale_vals <- c("yearly"="slategray3","decadal"="blue",
                    "baseline"="black", "hottest"="red",
                    "last"="darkgreen",
                    "last.month"="purple")
    scale_breaks <-  c("yearly","decadal","baseline",
                       "hottest","last","last.month")
    scale_labs <- c("Annual temp","10-year average",
                    basetitle, warmtitle,last_pt,last_m_pt)
  } else {
    aes_list <- list(shape=c(16,NA,NA,16,16),
                     linetype=c(0,1,2,0,0),
                     size=c(4,2,1,4,4),
                     lwd=c(1,2,1,1,1))
    scale_vals <- c("yearly"="slategray3","decadal"="blue",
                    "baseline"="black", "hottest"="red",
                    "last"="darkgreen")
    scale_breaks <-  c("yearly","decadal","baseline",
                       "hottest","last")
    scale_labs <- c("Annual temp","10-year average",
                    basetitle, warmtitle,last_pt)
  }
  gisplot <- gisplot + scale_color_manual("Temperatures",
                                          values= scale_vals,
                                          breaks= scale_breaks,
                                          labels= scale_labs)
  gisplot <- gisplot + guides(color=guide_legend(override.aes=aes_list,
                                                 keywidth=3))
  gisplot <- gisplot + labs(title = paste0("Global Temperature Anomalies (",
                                           min_year, " to ", max_year, ")" ),
                            x  = "Year",
                            y = expression(paste("Temp. Anomaly (", degree*C, ")", sep="")))
  gisplot <- gisplot + theme_gdocs(base_size=base_size)
  gisplot <- gisplot + theme(panel.grid.major=element_line(color="gray90"),
                             plot.title = element_text(hjust = 0.5),
                             axis.title.y = element_text(angle=90),
                             legend.position = c(0.21,0.58),
                             legend.text = element_text(size=base_size * 2./3.),
                             legend.key.size = unit(1.0, 'cm'),
                             legend.title=element_text(face="bold"),
                             legend.background=element_rect(fill='white',color='gray95'))
  gisplot <- gisplot + annotate("text",
                                x=max(monthly_data$year), y=min_temp,
                                label=data.source.text, color="darkgray",
                                hjust=1, size=base_size / 5)
  warm.string <- paste(n_warmest, "warmest years:", toString(warmest$year[1:warm_row[1]]))
  if (n_warmest > warm_row[1]) {
  for(i in seq(warm_row[1], n_warmest - 1, warm_row[2])) {
    j <- min(warm_row[2], n_warmest - i)
    warm.string <- paste(warm.string, '\n', toString(warmest$year[i + (1:j)], sep=''))
  }
  }
  gisplot <- gisplot + annotate('text', x=1950, y=max_temp - warm_row_pos,
                                hjust=0.5, vjust="top", size=base_size / 3,
                                label = warm.string, colour='darkred',fontface='bold')

  gisplot
}

make_gisplot_plain <- function(data, min_year = NULL, n_last = 15,  dec.nov=FALSE, base_size = 30) {
  data.source.text <- data$label
  monthly_data <- data$data
  bl <- (monthly_data %>% filter(year >= min(baseline_years) & year <= max(baseline_years)) %>%
           summarize(baseline = mean(t.anom, na.rm=T)))$baseline[[1]]
  monthly_data <- monthly_data %>%
    mutate(t.anom = t.anom - bl, t.anom.annual = t.anom.annual - bl,
           t.anom.decadal = t.anom.decadal - bl)
  annual_data <- extract_years(monthly_data, dec.nov) %>% filter(! is.na(t.anom.annual))
  GISS_last <- nrow(annual_data)
  GISS_last_yr <- annual_data$Year[GISS_last]
  GISS_last_temp <- annual_data$Temp[GISS_last]

  annual_data$time.frame <- 'yearly'
  annual_data$time.frame[nrow(annual_data) + 1 - n_last:1] <- 'last'
  annual_data$time.frame <- factor(annual_data$time.frame, levels = c('yearly', 'last', 'decadal'))

  annual_data <- annual_data %>% filter(year >= min_year)

  basetitle = paste0("Baseline (", baseline_years[1], "-", baseline_years[2], ")")

  gisplot <- ggplot(annual_data %>% filter(year >= min_year), aes(x = time, y = t.anom.annual, color=time.frame))
  gisplot <- gisplot + geom_point(size=I(6))
  gisplot <- gisplot + geom_line(size=I(1.5))
  gisplot <- gisplot + geom_line(aes(y = t.anom.decadal, color="decadal"), size=I(3))
  gisplot <- gisplot + scale_color_manual("Temperatures",
                                          values=c("yearly"="slategray3","decadal"="blue",
                                                   "last"="red"),
                                          breaks=c("yearly","decadal","last"),
                                          labels=c("Annual temp","10-year average",
                                                   sprintf("Last %d years", n_last)))
  gisplot <- gisplot + scale_y_continuous(breaks=seq(-1,1,0.1))
  gisplot <- gisplot + guides(color=guide_legend(override.aes=list(shape=c(16,NA,16),
                                                                   linetype=c(0,1,0),
                                                                   size=c(6,3,6)),
                                                 keywidth=3))
  gisplot <- gisplot + labs(title = paste("Global Temperature Anomalies (", min_year, " to ",
                                          GISS_last_yr,")",sep=""),
                            x  = "Year", y = expression(paste("Temperature Anomaly ",(degree*C))))
  gisplot <- gisplot + theme_gdocs(base_size=base_size)
  gisplot <- gisplot + theme(legend.position=c(.05,.85), legend.justification=c(0,1),
                             panel.grid.major=element_line(color="gray90"),
                             axis.title.y = element_text(angle=90),
                             plot.title = element_text(hjust=0.5),
                             legend.text = element_text(size=base_size),
                             legend.title=element_text(size=base_size,face="bold"))
  gisplot <- gisplot + annotate("text",x=2014,y=min(annual_data$t.anom.annual),
                                label=data.source.text, hjust=1, size=4.2)

  # gisplot <- gisplot + theme(legend.position=c(.15,.75))
  print(gisplot)
}

