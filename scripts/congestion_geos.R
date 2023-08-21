#congestion into zones
library(tidyverse)
library(sf)
library(here)
library(leaflet)

congestion_2018 <- read_rds(here::here("inputs", "congestion_2018.RDS"))

congestion_2018_geo <- congestion_2018 %>%
  select(region_id, west, east, south, north) %>%
  distinct(region_id, .keep_all = TRUE) %>%
  arrange(region_id)

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


congestion_regions[1, ]$box

map_of_regions <- leaflet() %>%
  addTiles()

for(regions in 1:nrow(congestion_regions)){
  print(regions)
  map_of_regions <- map_of_regions %>%
    addPolygons(data = congestion_regions[regions, ]$box,
                label = congestion_regions[regions, ]$region_id,
                color = "#1C110A",
                weight = .75,
                opacity = .5)
}

congestion_zone <- map_of_regions %>%
  addLayersControl()

