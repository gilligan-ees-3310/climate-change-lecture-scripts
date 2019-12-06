#
# Convective Greenhouse
#

library(ggplot2)
library(RColorBrewer)

if (! exists('tbr_earth')) source('lecture_utils.R')

conv.greenhouse.t <- function(delta.h = NA, lapse = elr_earth,
                              tskin = tbr_earth, tground0 = T_surf_earth,
                              z.ratio = NA, delta.t = NA,
                              lapse.ratio = 1.0) {
  z0 <- (tground0 - tskin) / lapse
  if (all(is.na(c(delta.h, z.ratio, delta.t)))) {
    z1 <- z0
    tground1 <-  tskin + z1 * lapse * lapse.ratio
  } else {
    if (!is.na(delta.h)) {
      z1 <- z0 + delta.h
      tground1 <- tskin + z1 * lapse * lapse.ratio
    } else if (! is.na(z.ratio)) {
      z1 <- z0 * z.ratio
      delta.h <- z1 - z0
      tground1 <- tskin + z1 * lapse * lapse.ratio
    }
    else if (! is.na(delta.t)) {
      tground1 <- tground0 + delta.t
      z1 <- z0 + delta.t / (lapse * lapse.ratio)
    }
  }
  list(t.ground.0=tground0, t.ground.1=tground1,
       delta.t =tground1-tground0,
       z0=z0, z1=z1,
       lapse=lapse, t.skin=tskin, lapse.ratio = lapse.ratio)
}

plot.conv.greenhouse <- function(delta.h = NA, lapse = 6.5, tskin = 254,
                                 reserve = NA,
                                 tground0 = 295,
                                 z.ratio = NA, delta.t = NA,
                                 lapse.ratio = 1.0,
                                 angle = NA, lab.adj = c(0,0),
                                 plot.warming = ! is.na(delta.h),
                                 lapse.feedback = FALSE,
                                 adiabat.labels = c("Moist adiabat",
                                                    "Moist adiabats")) {
  gh <- conv.greenhouse.t(delta.h, lapse, tskin, tground0, z.ratio, delta.t,
                          lapse.ratio)
  z0 <- gh$z0
  z1 <- gh$z1
  tground0 <- gh$t.ground.0
  tground1 <- gh$t.ground.1
  delta.t <- tground1 - tground0
  moist.gh <- data.frame(z=c(z0, z1, 0, 0),
                         t = c(tskin,tskin, tground0, tground1),
                         type=c('skin','skin','ground','ground'),
                         condition=rep(c('initial','added.ghg'),times=2))
  moist.gh$condition <- factor(moist.gh$condition,
                               levels=c('initial', 'added.ghg'), ordered=TRUE)

  if (! is.na(reserve)) {
    z.max = z0 * reserve
    t.max = tground0 + lapse * (z.max - z0)
  } else {
    z.max = max(z0, z1)
    t.max = max(tground0, tground1)
  }

  if (! plot.warming) {
    plot.data <- moist.gh[moist.gh$condition == 'initial',]
    x_labels <- c(expression(T[skin]), expression(T[ground]))
    x_breaks <- c(tskin, tground0)
    y_labels <- c('Skin height')
    y_breaks <- c(z0)
    adiabat.label <- adiabat.labels[1]
  } else {
    plot.data <- moist.gh
    adiabat.label <- adiabat.labels[2]
    x_labels <- c(expression(T[skin]), expression(T[ground]),
                  expression(paste("New ", T[g])))
    x_breaks <- c(tskin, tground0, tground1)
    if (lapse.feedback) {
      y_labels <- c('Skin height')
      y_breaks <- c(z0)
    } else {
      y_labels <- c('Skin height', 'New skin\nheight')
      y_breaks <- c(z0,z1)
    }
  }
  p <- ggplot(data=plot.data, aes(x=t,y=z, color=condition))
  p <- p + geom_vline(xintercept=tskin, lty="dashed") +
    geom_line(size=I(1)) +
    geom_point(aes(x=t,y=z),
               data=plot.data[plot.data$type=='skin',], pch=I(16), size=I(5))
  p <- p + scale_color_manual(values=brewer.pal(3,"Dark2"), guide="none") +
    scale_x_continuous(limits=c(tskin,t.max + 3),
                       breaks=x_breaks, labels=x_labels) +
    scale_y_continuous(expand=c(0,0),limits=c(0,z.max * 1.1),
                       breaks=y_breaks, labels = y_labels) +
    labs(x="Temperature",y="Altitude")
  if (lapse.feedback) {
    p <- p +  geom_text(label=adiabat.labels[1],
                        x = tskin + (tground0-tskin) * 3/4,
                        y=z0/4 * 1.1 + lab.adj[1], angle=-angle[1], color='black', size=7,
                        vjust = 0, hjust = 1)
    if (plot.warming) {
      p <- p +  geom_text(label=adiabat.labels[2],
                          x = tskin + (tground1 - tskin) * 3/4,
                          y=z0/4 * 0.9 + lab.adj[2], angle=-angle[2], color='black', size=7,
                          vjust=1, hjust = 1)
    }
  } else {
    if (! is.na(angle))
      p <- p +  geom_text(label=adiabat.label, x = mean(c(tskin,tground0)),
                          y=z1/2 + lab.adj[2], angle=-angle, color='black', size=7,
                          vjust = 0.5, hjust=0.5)
  }
  p <- p + theme_classic() +
    theme(text=element_text(size=20), axis.text=element_text(size=20))
  print(p)
}
