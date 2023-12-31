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
    
  
  
###
  
divvy_growth_projections <- divvy_2018 %>% 
  select(1:3, "user_type") %>%
  mutate(month = month(start_time, label = TRUE),
         year = year(start_time),
         month_year = paste(month, 
                            year,
                            sep = "-")) %>%
  filter(user_type != "Dependent") %>%
  select(-2, -3) %>%
  group_by(month, year, user_type) %>%
  summarize(count = n()) %>%
  select(year, month, user_type, count)


divvy_growth_projections <- divvy_growth_projections %>%
  pivot_wider(names_from = c(year), values_from = c(count))

divvy_growth_projections_subscriber <- divvy_growth_projections %>%
  filter(user_type == "Subscriber") %>%
  mutate(percent_growth = ((`2019` - `2018`) / `2018`) * 100)

divvy_growth_projections_customer <- divvy_growth_projections %>%
  filter(user_type == "Customer") %>%
  mutate(percent_growth = ((`2019` - `2018`) / `2018`) * 100)


# aggregate stations ------------------------------------------------------
divvy_from_counts <- divvy_2018 %>%
  select(start_time, from_station_id, user_type) 

divvy_to_counts <- divvy_2018 %>%
  select(start_time, to_station_id, user_type) 

%>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(from_station_id, user_type, weekday) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup()


format(divvy_2018$start_time, format = "%H:%M")

divvy_ride_time <- divvy_2018 %>%
  select(from_station_id, user_type, start_time) %>%
  mutate(time = format(start_time, format = "%H:%M"))

morning_rush <- divvy_ride_time %>%
  filter(time >= "04:30" & time <= "08:30")  %>%
  mutate(weekday = wday(start_time, label = TRUE))

afternoon_rush <- divvy_ride_time %>%
  filter(time >= "16:30" & time <= "19:00")
