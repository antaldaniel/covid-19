

load (  file = "data/google_distancing.rda")
trust <-read.csv ( 'data-raw/trust_institutions.csv', header=TRUE, 
                   stringsAsFactors = F) %>%
  mutate ( n = gsub(",","", n)) %>%
  mutate_at ( vars(-one_of('country_code')), ~as.numeric(gsub(",", "", .))) %>%
  mutate ( country_code = stringr::str_trim(country_code, side = 'both'))

names ( trust )
germany <- trust %>% filter (country_code %in% c("DE-E", "DE-W") ) 

t(colSums(germany[,2:4]))
  
germany <- data.frame ( country_code = rep("DE", 3),
                        names = c("trust", "not_trust", "dk"),  
                        values  = colSums(germany[,2:4]), 
                        stringsAsFactors = F
                        ) %>%
  spread ( names, values  ) %>%
  mutate ( n = trust + not_trust + dk)


trust_distance <- trust %>% rbind (germany) %>%
  mutate ( trust_level = (trust-not_trust) / n ) %>%
  left_join ( df %>% mutate_if(is.factor, as.character) )

names (trust_distance ) 
summary(lm ( trust_level ~ retail_and_recreation, data = trust_distance))

plot ( trust_distance$parks, trust_distance$trust_level)
library(ggplot2)
ggplot ( data = trust_distance, 
         aes ( x = workplaces, 
               y = trust_level )) +
  geom_point ( )

summary ( trust_distance$parks )

View ( trust_distance )
