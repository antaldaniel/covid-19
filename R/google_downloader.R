require(purrr)
google_directory <- file.path('data-raw', 'google')


'https://www.gstatic.com/covid19/mobility/2020-03-29_NL_Mobility_Report_en.pdf'

i = 1
download_date <- "2020-03-29"


if (! dir.exists( file.path(google_directory, download_date))) {
  dir.create(file.path(google_directory, download_date))
}
countries <- readRDS(file.path('data-raw', "countries.rds"))

downloader <- function(iso2c, download_date ) {

  ## Is the download directory exist?
  if (! dir.exists( file.path(google_directory, download_date ))) {
    dir.create(file.path(google_directory, download_date ))
  }

  ## Create url and file name
  download_url <- paste0("https://www.gstatic.com/covid19/mobility/",
                         download_date , "_",
                         iso2c, "_Mobility_Report_en.pdf")

  file_name <- file.path(google_directory, download_date,
                         paste0(iso2c, '.pdf'))

  ## If not downloaded yet, download it
  if ( ! file.exists(file_name) ) {
    download.file(url = download_url,
                  destfile = file_name,
                  method = 'wininet',
                  mode = 'wb')
  } else {
    message ( file_name, " already exists.")
  }
}

possibly_download <- purrr::possibly(downloader, NULL)


sapply ( countries$iso2c, function(x) possibly_download(x, download_date))

