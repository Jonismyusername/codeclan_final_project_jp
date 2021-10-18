library(tidyverse)
library(leaflet)
library(rgdal)

spdf <- readOGR(dsn = getwd(), layer = "la_zones_simple")
head(spdf)