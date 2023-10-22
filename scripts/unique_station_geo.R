#
library(tidyverse)
library(sf)
library(here)

# get rds -----------------------------------------------------------------

divvy_2018_to_geo <- read_rds(here::here("inputs", "divvy_2018_to_geo.RDS"))
divvy_2018_from_geo <- read_rds(here::here("inputs", "divvy_2018_from_geo.RDS"))


# create unique stations --------------------------------------------------


divvy_from <- divvy_2018_from_geo %>%
  st_drop_geometry() %>%
  distinct(from_station_id)



divvy_to <- divvy_2018_to_geo %>%
  st_drop_geometry() %>%
  distinct(to_station_id)

divvy_stations <- divvy_from %>%
  inner_join(divvy_to, by = c( "from_station_id" = "to_station_id"))


divvy_station_geo <- divvy_from %>%
  left_join(divvy_2018_from_geo) %>%
  select(-trip_id, -bike_id) %>%
  unique() %>%
  rename(station_id = from_station_id)
  
divvy_station_geo %>% 
  group_by(from_station_id) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))


write_rds(divvy_station_geo, here("inputs", "divvy_station_geo.RDS"))

