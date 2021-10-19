library(tidyverse)
library(leaflet)
library(rgdal)
library(sf)

spdf <- readOGR("clean_data/la_zones")

spdf <- st_as_sf(spdf) 

# spdf %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   # setView(lat = 10, lng = 0, zoom = 2) %>% 
#   addPolygons(label = ~name,
#               labelOptions = labelOptions(textsize = "15px"),
#               highlight = highlightOptions(weight = 5,
#                                color = "red",
#                                fillOpacity = 0.7,
#                                bringToFront = TRUE))


# shp_df <- broom::tidy(spdf, region = "code")

# green_map <- sp::merge(green_clean_2019, spdf2, by.x = "feature_code", by.y = "code")

green_clean_2019 <- green_clean %>% 
  filter(date_code == 2019)

green_map <- green_clean_2019 %>% 
  filter(feature_code != "S92000003") %>% 
  left_join(spdf, by = c("feature_code" = "code"))

community_clean_2019 <- community_clean %>% 
  filter(date_code == 2019)

community_map <- community_clean_2019 %>% 
  filter(feature_code != "S92000003") %>% 
  left_join(spdf, by = c("feature_code" = "code"))

neighbourhood_clean_2019 <- neighbourhood_clean %>% 
  filter(date_code == 2019)

neighbourhood_map <- neighbourhood_clean_2019 %>% 
  filter(feature_code != "S92000003") %>% 
  left_join(spdf, by = c("feature_code" = "code"))

# maps_access_to_green_space ----------------------------------------------

# ACCESS TO GREEN SPACE
# All
green_map %>% 
  filter(distance_to_green_space == "5 minute walk or less") %>% 
  filter_at(vars(age:ethnicity), all_vars(. == "All")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = value))+
  scale_fill_viridis_c(option = "magma")+
  labs(
    title = "Percentage of respondents living within a five minute walk to green or blue space",
    subtitle = "Based on 2019 data"
  )

# 65 years and over
green_map %>% 
  filter(distance_to_green_space == "5 minute walk or less"
         & age == "65 years and over") %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = value))+
  scale_fill_viridis_c(option = "magma")

# Pensioners
green_map %>% 
  filter(distance_to_green_space == "5 minute walk or less"
         & household_type == "Pensioners") %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = value))+
  scale_fill_viridis_c(option = "magma")

# Ethnicity
green_map %>% 
  filter(distance_to_green_space == "5 minute walk or less"
         & ethnicity == "Other") %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = value))+
  scale_fill_viridis_c(option = "magma")

# Table for all
green_map %>% 
  filter(distance_to_green_space == "5 minute walk or less"
         & date_code == 2019) %>% 
  filter_at(vars(age:ethnicity), all_vars(. == "All")) %>% 
  arrange(value) %>% 
  head(10)

# Table for ethnicity: other
green_map %>% 
  filter(distance_to_green_space == "5 minute walk or less"
         & date_code == 2019
         & ethnicity == "Other") %>% 
  arrange(value) %>% 
  head(10)


# maps_community_belonging ------------------------------------------------

# COMMUNITY

community_map %>% 
  filter(community_belonging == "Very strongly") %>% 
  filter_at(vars(gender:ethnicity), all_vars(. == "All")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = value))+
  scale_fill_viridis_c(option = "magma")+
  labs(
    title = "Percentage of respondents who felt 'Very strongly' that they belonged 
to their immediate community",
    subtitle = "Based on 2019 data"
  )



# maps_neighbourhood_rating -----------------------------------------------

neighbourhood_map %>% 
  filter(neighbourhood_rating == "Very good") %>% 
  filter_at(vars(gender:ethnicity), all_vars(. == "All")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = value))+
  scale_fill_viridis_c(option = "magma")+
  labs(
    title = "Percentage of respondents who rated their neighbourhood 'Very good'",
    subtitle = "Based on 2019 data"
  )
