#sf_rush
library(sf)
library(tidyverse)
library(readr)
library(here)

evening_rush <- read_rds(here("inputs", "evening_rush.RDS"))
morning_rush <- read_rds(here("inputs", "morning_rush.RDS"))
congestion_2018 <- read_rds(here::here("inputs", 
                                       "congestion_2018.RDS"))
congestion_geos <- source(here::here("scripts", 
                                     "congestion_geos.R"))

congestion_regions = tibble()
for(geo in 1:nrow(congestion_2018_geo)){
  box <- st_bbox(c(xmax = congestion_2018_geo[geo, ]$east,
                   xmin = congestion_2018_geo[geo, ]$west,
                   ymax = congestion_2018_geo[geo, ]$north,
                   ymin = congestion_2018_geo[geo, ]$south),
                 crs = 4326)
  box <- st_as_sfc(box)
  regions <- tibble(congestion_2018[geo, ]$region_id, box) 
  congestion_regions <- rbind(congestion_regions, regions)
}

congestion_regions <- congestion_regions %>%
  rename(region_id = `congestion_2018[geo, ]$region_id`) %>%
  arrange(region_id)

congestion_regions |>
  glimpse()


evening_rush_sf <- evening_rush %>%
  select(from_station_id, from_station_name, from_latitude, from_longitude) %>%
  filter(!is.na(from_longitude)) %>%
  st_as_sf(coords = c("from_longitude", "from_latitude"), crs = 4326)

evening_rush_counts <- congestion_regions %>%
  mutate(counts = lengths(st_intersects(congestion_regions$box, 
                                       evening_rush_sf$geometry)))  %>%
  arrange(desc(counts))
  

 evening_rush_counts %>% arrange(desc(count))
  
  
  
