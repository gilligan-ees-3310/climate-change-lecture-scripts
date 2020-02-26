library(httr)
library(rjson)

query_munich_re <- function(start_year = 1980, end_year = 2018,
                            event_categories = c("weather", "hydro", "climate")) {
  event_dict = c(geophysical = 1,
                 weather = 4, meteorological = 4,
                 hydro = 5, hydrological = 5,
                 climate = 7, climatological = 7)
  hdrs <- c("User-Agent"      =  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:73.0) Gecko/20100101 Firefox/73.0",
            "Origin"          = "https://natcatservice.munichre.com",
            "DNT"             = "1",
            "Referer"         = "https://natcatservice.munichre.com/?filter=eyJ5ZWFyRnJvbSI6MTk4MCwieWVhclRvIjoyMDE3fQ%3D%3D&type=1",
            "Pragma"          = "no-cache",
            "Cache-Control"   = "no-cache")
  url <- "https://natcatservice.munichre.com/api/analysis/1"
  if ("all" %in% event_categories) {
    event_categories <- NULL
  }
  if (! is.null(event_categories)) {
    event_categories = unlist(event_categories)
    if (is.character(event_categories)) {
      event_categories = event_dict[event_categories]
    } else if (! is.integer(event_categories)) {
      stop("event_categories must be character or integer. Instead it's [",
           str_c(class(event_categories), collapse = ", "), "]")
    }
  }

  body <- list(yearFrom = 1980, yearTo = 2018)
  if (! is.null(event_categories)) {
    body <- c(body, eventFamilyIds = event_categories)
  }

  result <- POST(url, body = body, config = add_headers(hdrs), encode = "json")
  if (result$status_code != 200) {
    stop("ERROR: Bad result from MunichRe: ", result$status_code)
  }
  data <- fromJSON(rawToChar(result$content))
  invisible(data)
}
