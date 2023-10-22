#scratch gis functions
library(tidyverse)
library(sf)
library(leaflet)

divvy_2018 <- readRDS(here::here("inputs", 
                                 "divvy_2018_data.RDS"))
congestion_2018 <- read_rds(here::here("inputs", 
                                       "congestion_2018.RDS"))
congestion_geos <- source(here::here("scripts", 
                                     "congestion_geos.R"))
divvy_stations <- read_rds(here::here("inputs", 
                                      "divvy_station_name_id.RDS"))

divvy_from <- read_rds(here::here("inputs", "divvy_2018_from_geo.RDS"))

total_rides <- divvy_2018 |> 
  distinct(trip_id) |>
  nrow()

total_observations <- nrow(congestion_2018) %>%
  format(big.mark=",")

divvy_2018 <- divvy_2018 %>% 
  filter(start_time >= min(congestion_2018$time))

divvy_from_counts <- divvy_2018 %>%
  select(start_time, from_station_id, bike_id, user_type) 

divvy_to_counts <- divvy_2018 %>%
  select(start_time, to_station_id, bike_id, user_type) 


left_join(divvy_from, divvy_from_counts, by = c("from_station_id" = "from_station_id", "bike_id" = "bike_id"))