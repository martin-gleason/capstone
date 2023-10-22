#load divvy
library(tidyverse)
library(RSocrata)
library(janitor)
library(sf)
library(leaflet)
library(lubridate)

#reading and cleaning divvy
divvy <- read_csv(here::here("inputs", "Divvy_Trips.csv")) %>%
  janitor::clean_names()

divvy <- divvy %>%
  mutate(start_time = parse_date_time(start_time, 
                                      orders = "%m/%d/%Y %I:%M:%S %p"))

divvy <- divvy %>%
  mutate(stop_time = parse_date_time(stop_time, 
                                     orders = "%m/%d/%Y %I:%M:%S %p"))

divvy <- divvy %>%
  mutate(start_year = year(start_time))

unique(divvy$start_year)

divvy_min <- min(divvy$start_time)
divvy_max <- max(divvy$start_time)

nrow(divvy)

divvy_2018 <- divvy %>%
  filter(start_year >= 2018)

glimpse(divvy_2018)

divvy_2018_data <- divvy_2018 %>%
  select(trip_id:birth_year)

glimpse(divvy_2018_data)

#congestion redaing and cleaning
congestion <- read_csv(here::here("inputs", "chicago_traffic.csv")) %>%
  janitor::clean_names()
congestion$time <- parse_date_time(congestion$time, orders = "%m/%d/%Y %I:%M:%S %p")

congestion <- congestion %>%
  mutate(start_year = year(time))

min(congestion$time)

congestion_2018 <- congestion %>%
  filter(start_year >= 2018 &
           start_year < 2020)

#sepearte divvy and divvy_geo
glimpse(divvy_2018)

divvy_2018_from_geo <- divvy_2018 %>%
  select(trip_id, bike_id, from_station_id, from_longitude, from_latitude) %>%
  filter(!is.na(from_longitude) | !is.na(from_latitude)) %>%
  st_as_sf(coords = c("from_longitude", "from_latitude"),
           crs = 4326)

divvy_2018_to_geo <- divvy_2018 %>%
  select(trip_id, bike_id, to_station_id, to_longitude, to_latitude) %>%
  filter(!is.na(to_longitude) | !is.na(to_latitude))%>%
  st_as_sf(coords = c("to_longitude", "to_latitude"),
           crs = 4326)


divvy_station_name_id <- divvy_2018 %>%
  select(from_station_id, from_station_name) %>%
  distinct(from_station_id, .keep_all = TRUE)

divvy_2018_from_geo %>%
  head(n = 100) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers()


divvy_2018_to_geo <- divvy_2018 %>%
  select(trip_id, to_location)

divvy_2018_from_geo %>%
  st_as_sf(coords = from_location,
           crs = 4326)


divy_historical <- divvy %>%
  filter(start_year <= 2017)

historical_divvy_data <- divy_historical %>%
  mutate(day = wday(start_time, label = TRUE)) %>%
  group_by(start_year, day, user_type) %>%
  summarise(Count = n())

options(scipen = 999)
historical_divvy_data %>%
  ggplot(aes(x = day, y = Count, fill = user_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~start_year) +
  theme_minimal()
  

# to and from morning rush---------------------------------------------------------

test <- head(divvy_2018, 100)

wday(test$start_time, label = TRUE)

work_day <- divvy_2018 %>%
  mutate(day = wday(start_time, label = TRUE),
         )

sundays <- work_day %>%
  filter(day == "Sun") %>%
  nrow()

saturday <- work_day %>%
  filter(day == "Sat") %>%
  nrow()

weekend <- saturday + sundays


no_weekends <- work_day %>%
  filter(day != "Sat") %>%
  filter(day != "Sun")

no_weekends %>% nrow()


test %>%
  mutate(time = hms::as_hms(paste0(hour(start_time), ":", minute(start_time),
                              ":", seconds(start_time)))) %>%
  filter(between(time, hms::as_hms("06:41:00"), hms::as_hms("06:48:00"))) %>%
  #filter(time <= hms::as_hms("06:41:00") & time <= hms::as_hms("06:48:00")) %>%
  select(time) %>%
  View()

morning_rush <- no_weekends %>%
  mutate(time = hms::as_hms(paste0(hour(start_time), ":", 
                                   minute(start_time),":", 
                                   seconds(start_time))))  %>%
  filter(between(time, hms::as_hms("05:00:00"), hms::as_hms("08:01:00"))) 

evening_rush <- no_weekends %>%
  mutate(time = hms::as_hms(paste0(hour(start_time), ":", 
                                   minute(start_time),":", 
                                   seconds(start_time))))  %>%
  filter(between(time, hms::as_hms("16:00:00"), hms::as_hms("18:01:00"))) 
  

# RDS  --------------------------------------------------------------------


write_rds(congestion_2018, here::here("inputs", "congestion_2018.RDS"))
write_rds(divvy_2018_data, here::here("inputs", "divvy_2018_data.RDS"))
write_rds(divvy_2018_from_geo, here::here("inputs", "divvy_2018_from_geo.RDS"))
write_rds(divvy_2018_to_geo, here::here("inputs", "divvy_2018_to_geo.RDS"))
write_csv(historical_divvy_data, here::here("outputs", "historical_divvy_data.csv"))
write_rds(divvy_station_name_id, here::here("inputs", "divvy_station_name_id.RDS"))
write_rds(morning_rush, here::here("inputs", "morning_rush.RDS"))
write_rds(evening_rush, here::here("inputs", "evening_rush.RDS"))
