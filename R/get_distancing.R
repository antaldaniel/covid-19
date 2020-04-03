library(pdftools)
library(tidyverse)

file_name <- file.path(google_directory, download_date,
                       paste0(iso2c, '.pdf'))

NL <- pdftools::pdf_text ( file.path(google_directory, download_date,
                                     paste0("NL", '.pdf')) ) %>%
  readr::read_lines() #open the PDF inside your project folder
PDF <- pdftools::pdf_text ( file.path(google_directory, download_date,
                                     paste0("NO", '.pdf'))) %>%
  readr::read_lines()

file_names <- dir (file.path(google_directory, download_date))

get_distancing_data <- function( file_name, download_date ) {

  country_code <- substr(file_name, 1,2)

  PDF <- pdftools::pdf_text ( file.path(google_directory, download_date,
                                        paste0(country_code, '.pdf'))) %>%
    readr::read_lines()

  data.frame (
    country_code = as.character(country_code),
    location_type = as.character(PDF[c(13,23,34,45,54,63)]),
    change = as.numeric(gsub("\\%", "", PDF[c(16,26,37,48,57,66)]))
    )
  }

possibly_get_distancing_data <- purrr::possibly(get_distancing_data, NULL)

distancing <- get_distancing_data (file_names[1], download_date = download_date)

for ( i in 2:length(file_names)) {

  tmp <- possibly_get_distancing_data (file_names[i], download_date = download_date)
  if(!is.null(tmp)) {
    message ( "Adding ", tmp$country_code[1])
    distancing <- rbind( distancing, tmp)
  } else {
    message ("Failed with ", file_names[i])
  }
     }

distancing_wide <- distancing %>%
  mutate ( location_type = gsub("\\&", "and", tolower(location_type))) %>%
  mutate ( location_type = gsub("\\s", "_", location_type)) %>%
  filter ( ! country_code %in% c("LI", "GW")) %>%
  spread ( location_type, change )

save ( distancing, distancing_wide, file = "data/google_distancing.rda")

