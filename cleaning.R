library(tidyverse)
library(janitor)

green_clean <- green %>%
  rename(distance_to_green_space = distance_to_nearest_green_or_blue_space) %>% 
  mutate(distance_to_green_space = 
           recode(distance_to_green_space,
                  "A 5 minute walk or less" = "5 minute walk or less",
                  "Within a 6-10 minute walk" = "6 - 10 minute walk",
                  "An 11 minute walk or more" = "11 minute walk or more")) %>% 
  filter(measurement == "Percent") %>% 
  select(-units)

community_clean <- community %>% 
  filter(measurement == "Percent") %>% 
  select(-units)

neighbourhood_clean <- neighbourhood %>% 
  mutate(neighbourhood_rating = 
           factor(neighbourhood_rating, 
                  levels = c("Very good",
                             "Fairly good",
                             "Fairly poor",
                             "Very poor",
                             "No opinion"),
                  ordered = TRUE)) %>%
  filter(measurement == "Percent") %>% 
  select(-units)
  
