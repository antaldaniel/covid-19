library(pdftools)


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

df <- distancing %>%
  mutate ( location_type = gsub("\\&", "and", tolower(location_type))) %>%
  mutate ( location_type = gsub("\\s", "_", location_type)) %>%
  filter ( ! country_code %in% c("LI", "GW")) %>%
  spread ( location_type, change )

save ( distancing, df, file = "data/google_distancing.rda")

library( regdata)

regdata::geodata_nuts0$id

residential <- df %>% select ( country_code, residential ) %>%
  filter ( country_code %in% c("GB", "GR", regdata::geodata_nuts0$id)) %>%
  mutate ( time = as.Date("2013-01-01"), 
           values = as.numeric(residential)) %>%
  rename ( geo = country_code ) %>%
  create_choropleth( ., level = 0,
                     base_color =  "#00348A" , reverse_scale = FALSE, 
                     unit_text = "Residential %", 
                     title = "Social Distancing in Europe")

residential <- residential + labs ( subtitle = "Change Staying in Residental Area", 
                                    caption = "                                                                (c) Daniel Antal, 2020. danielantal.eu, satellitereport.com 
               data: https://www.blog.google/technology/health/covid-19-community-mobility-reports") +
  theme ( plot.caption = element_text ( hjust = 0.2) )

residential

park <- df %>% select ( country_code, parks ) %>%
  filter ( country_code %in% c("GB", "GR", regdata::geodata_nuts0$id)) %>%
  mutate ( time = as.Date("2013-01-01"), 
           values = as.numeric(parks)) %>%
  rename ( geo = country_code ) %>%
  create_choropleth( ., level = 0,
                     base_color = "#3EA135" , reverse_scale = TRUE, 
                     unit_text = "Parks %", 
                     title = "Social Distancing in Europe")

park <- park + labs ( subtitle = "Change Staying in Parks", 
                              caption = "                                                                (c) Daniel Antal, 2020. danielantal.eu, satellitereport.com 
               data: https://www.blog.google/technology/health/covid-19-community-mobility-reports") +
  theme ( plot.caption = element_text ( hjust = 0.2) )

park


names ( df )
transit <- df %>% select ( country_code, transit_stations ) %>%
  filter ( country_code %in% c("GB", "GR", regdata::geodata_nuts0$id)) %>%
  mutate ( time = as.Date("2013-01-01"), 
           values = as.numeric(transit_stations)) %>%
  rename ( geo = country_code ) %>%
  create_choropleth( ., level = 0,
                     base_color =  "#FAE000"  , reverse_scale = TRUE, 
                     unit_text = "Public Transport %", 
                     title = "Social Distancing in Europe")

transit <- transit + labs ( subtitle = "Change Staying in Public Transport Stations (-)", 
                      caption = "                                                                (c) Daniel Antal, 2020. danielantal.eu, satellitereport.com 
               data: https://www.blog.google/technology/health/covid-19-community-mobility-reports") +
  theme ( plot.caption = element_text ( hjust = 0.2) )

transit

work <- df %>% select ( country_code, workplaces ) %>%
  filter ( country_code %in% c("GB", "GR", regdata::geodata_nuts0$id)) %>%
  mutate ( time = as.Date("2013-01-01"), 
           values = as.numeric(workplaces)) %>%
  rename ( geo = country_code ) %>%
  create_choropleth( ., level = 0,
                     base_color = "#5C2320" , reverse_scale = TRUE, 
                     unit_text = "Workplaces %", 
                     title = "Social Distancing in Europe")

work <- work + labs ( subtitle = "Change Staying in Workplaces", 
                      caption = "                                                                (c) Daniel Antal, 2020. danielantal.eu, satellitereport.com 
               data: https://www.blog.google/technology/health/covid-19-community-mobility-reports") +
  theme ( plot.caption = element_text ( hjust = 0.2) )

work
ggsave ( filename = 'google_location_residental_change-2020-03-29.jpg',
         plot = residential,
         unit = "cm", width = 15, height = 10)

ggsave ( filename = 'google_location_park_change-2020-03-29.jpg',
         plot = park,
         unit = "cm", width = 15, height = 10)

library(gridExtra)

residential_plot <- residential + labs ( title = NULL, caption  = NULL, subtitle = NULL )
work_plot <- work + labs ( title = NULL, caption  = NULL, subtitle = NULL )
park_plot <- park + labs ( title = NULL, caption  = NULL, subtitle = NULL )
transit_plot <- transit + labs ( title = NULL, caption  = NULL, subtitle = NULL )

library(grid)
g <- grid.arrange(
  residential_plot, park_plot,
  transit_plot, work_plot, nrow = 2,
  top = "Social Distancing in Europe: Change In Time Spent",
  bottom = textGrob(
    "                                                                (c) Daniel Antal, 2020. danielantal.eu, satellitereport.com 
               Data: https://www.blog.google/technology/health/covid-19-community-mobility-reports",
    gp = gpar(fontface = 1, fontsize = 9),
    hjust = 1,
    x = 1
  )
)

ggsave ( "grid.jpg", g)
