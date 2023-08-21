#load_clean_data
library(tidyverse)
library(sf)
library(here)

divvy_data_2018 <- read_rds(file.path("inputs", "divvy_2018_data.RDS"))
divvy_to_2018 <- read_rds(file.path("inputs", "divvy_2018_to_geo.RDS"))
divvy_from_2018 <- read_rds(file.path("inputs", "divvy_2018_from_geo.RDS"))
congestion_2018 <- read_rds(file.path("inputs", "congestion_2018.RDS"))

congestion_2018_nw <- congestion_2018 %>%
  st_as_sf(coords = c("north", "west"))

congestion_2018_se <- congestion_2018 %>%
  st_as_sf(coords = c("south", "east"))

congestion_2018_nw %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = congestion_2018_se)

congestion_north <- congestion_2018 %>%
  select(region, north)

st_as_sf(coords = congestion_north$north, crs = 4326)


  
## congestion box drawing

m <- congestion_2018 %>%
  head(1)
m

test_list <- list( m$east, m$west, m$north, m$south)

st_polygon(x = test_list, dim = "xy")

test <- st_bbox(c(xmax = m$east, xmin = m$west, 
                 ymax = m$north, ymin = m$south), crs = 4326)

test_2 <- test %>% st_as_sfc()

test %>% st_as_sfc() %>% leaflet() %>% addTiles() %>% addPolygons(data = test_2)
  

class(test)
test %>%
  ggplot() +
  geom_sf()
        
  group_by(region) %>%
  summarise(n = n())
  
  
  
  historical_divvy %>%
    ggplot(aes(x = fct_inorder(day), y = Count, fill = user_type, group = user_type, day)) +
    geom_area() +
    facet_wrap(~start_year) +
    labs(x = "Weekday", 
         y = "Number of Rides",
         fill = "Type of Customer",
         title = "Divvy Bike Rentals in Chicago: 2013 - 2017") +
    theme_minimal()
  
  
  historical_divvy %>%
    ggplot(aes(x = fct_inorder(day), y = Count, fill = user_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~start_year) +
    labs(x = "Weekday", 
         y = "Number of Rides",
         fill = "Type of Customer",
         title = "Divvy Bike Rentals in Chicago: 2013 - 2017") +
    coord_flip() +
    theme_minimal()
  
  
  
  
  historical_divvy %>%
    filter(user_type != "Dependent") %>%
    pivot_wider(names_from = start_year, values_from = Count) %>%
    View()
    
