## File Download and File
## GISS monthly data import script develodped by http://LearnR.wordpress.com

giss_url <- c(land.sea = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt",
              land     = "https://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts.txt")

giss_csv_url <- c(land.sea = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
                  land     = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts.csv")


giss_data.source.text <- c( land.sea = paste0("Data source: NASA, ", giss_url['land.sea']),
                            land = paste0("Data source: NASA, ", giss_url['land']))

giss_file <- c(land.sea = file.path(data.dir, 'global_temp', 'giss', "GLB.Ts+dSST.txt"),
               land = file.path(data.dir, 'global_temp', 'giss', 'GLB.Ts.txt'))

giss_csv_file <- c(land.sea = file.path(data.dir, 'global_temp', 'giss', "GLB.Ts+dSST.csv"),
               land = file.path(data.dir, 'global_temp', 'giss', 'GLB.Ts.csv'))

update.data <- function() {
  for (loc in c('land.sea','land')) {
    download.file(giss_url[loc], giss_file[loc])
    download.file(giss_csv_url[loc], giss_csv)
  }
}
