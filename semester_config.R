#
# Initialize semester directory info
#

find_semester_dir <- function(p = NULL) {
  if (is.null(p)) p <- getwd()
  p <- normalizePath(p)
  if (file.exists(file.path(p,'semester.yml'))) {
    return(p)
  } else {
    d <- dirname(p)
    if (d == p) return(NA)
    return(find_semester_dir(d))
  }
}


if(!exists('semester_dir') || ! dir.exists(semester_dir) || ! file.exists(file.path(semester_dir, 'semester.yml'))) {
#  message("Creating semester_dir")
  semester_dir <- find_semester_dir()
} else {
#  message("Semester dir exists:", semester_dir, '\n')
}

if(!exists('script_dir') || ! dir.exists(script_dir)) script_dir <- file.path(semester_dir, 'util_scripts')
if(!exists('data_dir') || ! dir.exists(data_dir)) data_dir <- file.path(semester_dir,'data')

#message('Data dir = ', data_dir, " and it ", ifelse(dir.exists(data_dir), 'exists.', 'does not exist.'))
