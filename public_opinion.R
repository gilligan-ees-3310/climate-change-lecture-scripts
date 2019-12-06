#
# Polling data on climate change, 
# Aggregated by pollingreport.com
# http://www.pollingreport.com/enviro.htm
#

library(tidyverse)
library(stringr)
library(lubridate)

get_public_opinion = function() {
  public_belief_anthropogenic = tibble(
    date = c(
      "2017-04-15", # CBS
      "2015-11-22", # CBS
      "2015-09-15", # CBS
      "2014-09-14", # CBS
      "2014-05-19", # CBS
      "2014-02-23" # CBS
    ),
    anthropogenic = c(
      53,
      53,
      51,
      54,
      49,
      46
    ),
    source = c(
      "CBS"
    )
  ) %>%
    bind_rows(
      tibble(
        date = c(
          "2017-03-05",
          "2016-03-06",
          "2015-03-08",
          "2014-03-09",
          "2013-03-10",
          "2012-03-11",
          "2011-03-06",
          "2010-03-07"
        ),
        anthropogenic = c(
          68,
          65,
          55,
          57,
          57,
          53,
          52,
          50
        ),
        source = c(
          "Gallup"  
        )
      )
    ) %>%
    bind_rows(
      tibble(
        date = c(
          "2015-05-06",
          "2013-02-17",
          "2012-10-07",
          "2011-11-14",
          "2011-03-01",
          "2010-10-18",
          "2009-10-04"
        ),
        anthropogenic = c(
          45,
          42,
          42,
          38,
          36,
          34,
          36
        ),
        source = c(
          "Pew"  
        )
      )
    ) %>%
    bind_rows(
      tibble(
        date = c(
          "2013-01-15",
          "2011-09-11",
          "2009-12-03",
          "2008-06-05",
          "2007-10-14",
          "2007-05-06"
        ),
        anthropogenic = c(
          49,
          48,
          45,
          54,
          56,
          54
        ),
        source = c(
          "CNN"  
        )
      )
    ) %>%
    bind_rows(
      tibble(
        date = c(
          "2012-03-11",
          "2011-03-06",
          "2010-03-07",
          "2008-03-09",
          "2007-03-14",
          "2006-03-16",
          "2003-03-05"
        ),
        anthropogenic = c(
          53,
          52,
          50,
          58,
          61,
          58,
          61
        ),
        source = c(
          "Gallup"
        )
      )
    ) %>%
    bind_rows(
      tibble(
        date = c(
          "2011-11-14",
          "2011-03-01",
          "2010-10-18",
          "2008-04-15",
          "2007-01-15",
          "2006-08-15",
          "2006-07-15",
          "2006-06-15"
        ),
        anthropogenic = c(
          38,
          36,
          34,
          47,
          47,
          47,
          50,
          41
        ),
        source = c(
          "Pew"
        )
      )
    ) %>%
    bind_rows(
      tibble(
        date = c(
          "2017-05-15",
          "2016-11-15",
          "2016-03-15",
          "2015-10-15",
          "2015-03-15",
          "2014-10-15",
          "2014-04-15",
          "2013-11-15",
          "2013-04-15",
          "2012-09-15",
          "2012-03-15",
          "2011-11-15",
          "2011-05-15",
          "2010-06-15",
          "2010-01-15",
          "2008-11-15"
        ),
        anthropogenic = c(
          56,
          55,
          56,
          52,
          52,
          48,
          51,
          48,
          47,
          52,
          46,
          48,
          46,
          48,
          46,
          55
        ),
        source = c(
          "Yale/George Mason"
        )
      )
    )
  
  invisible(public_belief_anthropogenic)
}

plot_public_opinion <- function(public_belief = public_belief_anthropogenic) {
  ggplot(public_belief, aes(x = ymd(date), y = anthropogenic, color = source)) + 
    geom_point(size = 2) + 
    geom_line(stat = "smooth", size = 1, alpha = 0.7, method = "auto") + 
    geom_hline(yintercept = 50, alpha = 0.50, color = "black") + 
    scale_color_brewer(palette = "Dark2") + 
    labs(x = "Year", y = "Caused by Humans (%)") + 
    guides(color = guide_legend(override.aes=list(alpha = 1.0))) +
    theme_bw(base_size = 20)
}
