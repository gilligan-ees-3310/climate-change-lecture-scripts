#
# Deal with Kaya variables
#
library(tidyverse)
library(readxl)

this.year <- as.POSIXlt(Sys.Date())$year + 1900

clean_names <- function(x) {
  x %>% str_replace_all(
    c('[^a-zA-Z0-9_]+' = '.',
      '\\.\\.+' = '.',
      '^\\.' = '',
      '\\.$' = '')) %>%
    invisible()
}

fix_pop_names <- function(x) {
  x %>% clean_names() %>%
    str_replace_all(
      fixed(c(
        'Ha.ti' = 'Haiti'
      ))
    )
}

fix_gdppc_names <- function(x) {
  x %>% clean_names() %>%
    str_replace_all(c('^([0-9])' =  'X\\1')) %>%
    str_replace_all(
      fixed(c(
        'Holland.Netherlands' = 'Netherlands',
        'England.GB.UK' = 'UK',
        'Indonesia.Java.before.1880' = 'Indonesia.Timor.until.99',
        'Byzantium.Ottoman.Empire.Turkey' = 'Turkey',
        'Sao.Tom.Principe' = 'S.Tom.P',
        'Cape.Colony.South.Africa' = 'South.Africa',
        'Congo.Kinshasa' = 'Zaire.Congo.Kinshasa',
        'Centre.North.Italy' = 'Italy',
        'Ha.ti' = 'Haiti',
        'Total.World' = 'World.Total'))
    ) %>%
    invisible()
}

fix_gdppc <- function(x) {
  x %>% setNames(., names(.) %>% fix_gdppc_names()) %>%
    mutate(Serbia.Montenegro.Kosovo = rowSums(cbind(Serbia, Montenegro, Kosovo), na.rm=TRUE)) %>%
    select(-Serbia, -Montenegro, -Kosovo)
}

fix_cdiac_names <- function(x) {
  x %>% clean_names() %>%
    str_replace_all(c('^([0-9])' =  'X\\1')) %>%
    str_replace_all(
      fixed(c(
        'China.Mainland' = 'China',
        'Democratic.People.S.Republic.Of.Korea' = 'N.Korea',
        'France.Including.Monaco' = 'France',
        'Hong.Kong.Special.Adminstrative.Region.Of.China' = 'Hong.Kong',
        'Islamic.Republic.Of.Iran' = 'Iran',
        'Italy.Including.San.Marino' = 'Italy',
        'New.Zealand' = 'N.Zealand',
        'Republic.Of.Korea' = 'S.Korea',
        'Russian.Federation' = 'Russia',
        'United.Arab.Emirates' = 'UAE',
        'United.Kingdom' = 'UK',
        'United.States.Of.America' = 'USA',
        'World' = 'World.Total'))
    ) %>%
    invisible()
}




clean_mad <- function(x) {
  x <- x %>% filter(!is.na(year))
  c <- x %>% summarize_all(funs(any(!is.na(.)))) %>% unlist() %>% which()
  x <- x %>% select(!!c)
  colnames(x) <- clean_names(colnames(x))
  invisible(x)
}

strip_blanks <- function(x) {
  x %>% purrr::discard(~.x == '')
}


fill_in <- function(df, this.year) {
  interp.years <- seq(max(df %>% filter(year < this.year) %>% select(year)) + 1,
                      this.year)
  output <- data.frame(year = interp.years)
  for (i in seq_along(df)[-1]) {
    output <- cbind(output, approx(na.omit(df[,c(1,i)]), xout = interp.years)$y)
  }
  names(output) <- names(df)
  output <- rbind(df %>% filter(year < this.year), output)
  invisible(output)
}

load_kaya <- function(data.dir = 'data') {

  pop <- read.csv(file.path(data.dir, 'kaya','vertical_pop.csv'),header=T,skip=2,
                  blank.lines.skip=TRUE, stringsAsFactors=FALSE,
                  colClasses=c('numeric'))
  names(pop)[1] <- 'year'
  pop <- pop %>% clean_mad() %>% setNames(., fix_pop_names(names(.))) %>%
    fill_in(this.year)

  kaya <- pop %>% gather(-year, key = nation, value = P)

  #gdp <- read.csv(file.path(data.dir, 'kaya', 'vertical_gdp.csv'),header=T,skip=2,
  #                blank.lines.skip=TRUE, stringsAsFactors=FALSE,
  #                colClasses=c('numeric'))
  #gdp <- gdp %>% clean_mad()
  #gdp <- fill_in(gdp, this.year)

  gdppc <- read_excel(file.path(data.dir, 'kaya', 'mpd_2013-01.xlsx'),skip=2)
  names(gdppc)[1] <- 'year'
  gdppc <- gdppc[,!is.na(names(gdppc))] %>%
    filter(! is.na(year)) %>%
    clean_mad() %>%
    fix_gdppc()

  kaya <- gdppc %>% gather(-year, key = nation, value = g) %>%
    full_join(kaya, ., by = c('year','nation')) %>%
    mutate(G = g * P)

  cdiac_global_filename <- 'Global_Carbon_Budget_2015_v1.1.xlsx'
  cdiac_national_filename <- 'National_Carbon_Emissions_2015_v1.1.xlsx'

  co2.nat <- read_excel(file.path(data.dir, 'kaya', cdiac_national_filename), col_names = TRUE,
                        sheet = 'Territorial Emissions CDIAC', skip=14)

  foo <- read_excel(file.path(data.dir, 'kaya', cdiac_national_filename), col_names = TRUE,
                    sheet = 'Territorial Emissions CDIAC', skip=13)
  names(co2.nat) <- str_to_title(strip_blanks(names(foo)))
  rm(foo)
  names(co2.nat)[1] <- 'year'
  co2.nat <- co2.nat[,!is.na(names(co2.nat))] %>%
    filter(! is.na(year)) %>%
    clean_mad() %>%
    setNames(., names(.) %>% fix_cdiac_names()) %>%
    gather(-year, key = nation, value = co2.nat)

  co2.glob <- read_excel(file.path(data.dir, 'kaya', cdiac_global_filename), col_names = TRUE,
                         sheet = 'Global Carbon Budget', skip=19)
  names(co2.glob)[1] <- 'year'
  co2.glob <- co2.glob[,!is.na(names(co2.glob)) & names(co2.glob) != ''] %>%
    filter(! is.na(year)) %>%
    clean_mad()

  co2.fuel <- read_excel(file.path(data.dir, 'kaya', cdiac_global_filename), col_names = TRUE,
                         sheet = 'Fossil Emissions by Fuel Type', skip=10)
  names(co2.fuel)[1] <- 'year'
  co2.fuel <- co2.fuel[,!is.na(names(co2.fuel)) & names(co2.fuel) != ''] %>%
    filter(! is.na(year)) %>%
    clean_mad()

  co2.xfer <- read_excel(file.path(data.dir, 'kaya', cdiac_national_filename), col_names = TRUE,
                         sheet = 'Emissions Transfers UNFCCC', skip=8)
  foo <- read_excel(file.path(data.dir, 'kaya', cdiac_national_filename), col_names = TRUE,
                    sheet = 'Emissions Transfers UNFCCC', skip=7)
  names(co2.xfer) <- str_to_title(names(foo))
  rm(foo)
  names(co2.xfer)[1] <- 'year'

  co2.xfer <- co2.xfer[,!is.na(names(co2.xfer)) & names(co2.xfer) != ''] %>%
    filter(! is.na(year)) %>%
    clean_mad() %>%
    setNames(., names(.) %>% fix_cdiac_names()) %>%
    gather(-year, key = nation, value = co2.xfer)

  co2.consum <- read_excel(file.path(data.dir, 'kaya', cdiac_national_filename), col_names = TRUE,
                           sheet = 'Consumption Emissions UNFCCC', skip=8)

  foo <- read_excel(file.path(data.dir, 'kaya', cdiac_national_filename), col_names = TRUE,
                    sheet = 'Consumption Emissions UNFCCC', skip=7)
  names(co2.consum) <- str_to_title(strip_blanks(names(foo)))
  rm(foo)
  names(co2.consum)[1] <- 'year'
  co2.consum <- co2.consum[! is.na(names(co2.consum)) & names(co2.consum) != ''] %>%
    filter(! is.na(year)) %>%
    clean_mad() %>%
    setNames(., names(.) %>% fix_cdiac_names()) %>%
    gather(-year, key = nation, value = co2.consum)

  co2.glob.hist <- read_excel(file.path(data.dir, 'kaya', cdiac_global_filename), col_names = TRUE,
                              sheet = 'Historical Budget', skip=12)
  names(co2.glob.hist)[1] <- 'year'
  co2.glob.hist <- co2.glob.hist[,!is.na(names(co2.glob.hist)) & names(co2.glob.hist) != ''] %>%
  filter(! is.na(year)) %>%
  clean_mad()

  co2.glob.hist <- co2.glob.hist %>% mutate(total = fossil.fuel.and.cement.emissions + land.use.change.emissions)

  co2.nat.hist <- read.csv(file.path(data.dir, 'kaya', 'nation.1751_2013.csv'),header=F,skip=3,
                           col.names=c('country','year','total','solid','liquid',
                                       'gas','cement','flaring','pc','bunker'))
  co2.nat.hist$country <- str_to_title(co2.nat.hist$country)

  co2.nat.hist <- co2.nat.hist[,!is.na(names(co2.nat.hist)) & names(co2.nat.hist) != ''] %>%
    filter(! is.na(year)) %>%
    clean_mad()

  kaya <- kaya %>% full_join(co2.nat, by = c('year','nation')) %>%
    full_join(co2.consum, by = c('year','nation')) %>%
    full_join(co2.xfer, by = c('year','nation'))

  list(kaya = kaya, co2.glob = co2.glob, co2.glob.hist = co2.glob.hist,
       co2.fuel = co2.fuel, co2.nat.hist = co2.nat.hist) %>%
    invisible()
}

