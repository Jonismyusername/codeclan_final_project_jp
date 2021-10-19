library(tidyverse)
library(janitor)

community <- read_csv("raw_data/community_belonging.csv") %>% 
  clean_names()

green <- read_csv("raw_data/green_spaces.csv") %>% 
  clean_names()

neighbourhood <- read_csv("raw_data/neighbourhood_rating.csv") %>% 
  clean_names()

data_zones <- read_csv("raw_data/Datazone2011lookup.csv") %>% 
  clean_names()

green_clean <- green %>%
  rename(distance_to_green_space = distance_to_nearest_green_or_blue_space) %>% 
  mutate(distance_to_green_space = 
           recode(distance_to_green_space,
                  "A 5 minute walk or less" = "5 minute walk or less",
                  "Within a 6-10 minute walk" = "6 - 10 minute walk",
                  "An 11 minute walk or more" = "11 minute walk or more")) %>%
  mutate(distance_to_green_space = 
           factor(distance_to_green_space, 
                  levels = c("5 minute walk or less",
                             "6 - 10 minute walk",
                             "11 minute walk or more",
                             "Don't Know"),
                  ordered = TRUE)) %>%
  filter(measurement == "Percent") %>% 
  select(-units)

community_clean <- community %>%
  mutate(community_belonging = 
           factor(community_belonging, 
                  levels = c("Very strongly",
                             "Fairly strongly",
                             "Not very strongly",
                             "Not at all strongly",
                             "Don't know"),
                  ordered = TRUE)) %>% 
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

data_zones_subset <- data_zones %>% 
  select(la_code, la_name) %>% 
  distinct()

data_zone_scotland <- data_zones %>% 
  filter(country_code == "S92000003") %>% 
  select(country_code, country_name) %>% 
  distinct()

green_clean_scotland <- green_clean %>% 
  filter(feature_code == "S92000003") %>% 
  inner_join(data_zone_scotland, by = c("feature_code" = "country_code")) %>% 
  rename("name" = "country_name")

green_clean_names <- green_clean %>% 
  filter(feature_code != "S92000003") %>% 
  inner_join(data_zones_subset, by = c("feature_code" = "la_code")) %>% 
  rename("name" = "la_name")

green_clean_names <- bind_rows(green_clean_names, green_clean_scotland)


community_clean_scotland <- community_clean %>% 
  filter(feature_code == "S92000003") %>% 
  inner_join(data_zone_scotland, by = c("feature_code" = "country_code")) %>% 
  rename("name" = "country_name")

community_clean_names <- community_clean %>% 
  filter(feature_code != "S92000003") %>%
  inner_join(data_zones_subset, by = c("feature_code" = "la_code")) %>% 
  rename("name" = "la_name")

community_clean_names <- bind_rows(community_clean_names, community_clean_scotland)
  

neighbourhood_clean_scotland <- neighbourhood_clean %>% 
  filter(feature_code == "S92000003") %>% 
  inner_join(data_zone_scotland, by = c("feature_code" = "country_code")) %>% 
  rename("name" = "country_name")

neighbourhood_clean_names <- neighbourhood_clean %>% 
  filter(feature_code != "S92000003") %>%
  inner_join(data_zones_subset, by = c("feature_code" = "la_code")) %>% 
  rename("name" = "la_name")

neighbourhood_clean_names <- bind_rows(neighbourhood_clean_names, neighbourhood_clean_scotland)


