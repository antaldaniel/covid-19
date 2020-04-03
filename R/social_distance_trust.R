library(tidyverse)
library(ggrepel)
srpalette <- readRDS(file.path("data-raw", "srpalette.rds"))

load (  file = "data/google_distancing.rda")
trust <- read.csv ( file.path("data-raw", "csv", "trust_institutions.csv"),
                    header=TRUE, stringsAsFactors = F) %>%
  purrr::set_names ( c("country_code", names(.)[2:5])) %>%
  mutate_at ( vars(-one_of('country_code')), ~as.numeric(gsub(",", "", .))) %>%
  mutate ( country_code = stringr::str_trim(country_code, side = 'both'))

sum_germany <- trust %>% filter (country_code %in% c("DE-E", "DE-W") )

germany <- data.frame ( country_code = rep("DE", 3),
                        names = c("trust", "not_trust", "dk"),
                        values  = colSums(sum_germany[,2:4]),
                        stringsAsFactors = FALSE ) %>%
  spread ( names, values  ) %>%
  mutate ( n = trust + not_trust + dk)

trust_distance <- trust %>%
  rbind (germany) %>%
  mutate ( trust_level = (trust-not_trust) / n ) %>%
  filter ( !is.nan(trust_level)) %>%
  left_join ( distancing_wide %>% mutate_if (is.factor, as.character),
              by = 'country_code') %>%
  filter ( !is.na(parks))

names ( trust_distance )
summary(lm ( trust_level ~ retail_and_recreation, data = trust_distance))

trust_distance %>%
  pivot_longer ( cols = c("parks", "transit_stations", "workplaces",
                          "retail_and_recreation", "grocery_and_pharmacy",
                          "residential"),
                 names_to = "places", values_to = "values") %>%
  mutate ( places = gsub("_", " ", places) ) %>%
  mutate ( places = gsub("and", "\\&", places )) %>%
  mutate ( places = gsub("transit stations", "public transport", places)) %>%
  ggplot ( data =  ,
         aes ( x = values,
               y = trust_level,
               color  = places,
               group  = places,
               label = country_code )) +
  geom_point () +
  geom_smooth( method = 'lm', formula = 'y ~ x') +
  scale_color_manual( values  = as.character(srpalette)) +
  geom_text_repel() +
    facet_wrap (facets = "places",
                ncol = 3,
                scales = 'free_x' ) +
  labs ( x = "Change in location type use (%)",
         y = "Trust level in national government",
         title  = "Political Trust & Social Distancing") +
  guides ( color = FALSE )

ggsave ( filename = file.path("plots", "trust_distancing_wrap.jpg"),
         units = "cm", width = 16*1.5, height = 9*1.5, dpi = 300)
