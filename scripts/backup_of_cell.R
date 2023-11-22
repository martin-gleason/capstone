{r}
#| label: from_evening_rush
#| warning: false
#| echo: false
#| cache: true

evening_counts <- evening_rush %>%
  mutate(start_date = as.Date(lubridate::ymd_hms(start_time)),
         month = lubridate::month(start_time, label = TRUE, abbr = FALSE),
         year = lubridate::year(start_time)) %>%
  group_by(from_station_id, start_date) %>%
  summarize(count = n())

congestion_regions %>% st_as_sf()

evening_summary <- evening_counts %>%
  group_by(from_station_id) %>%
  summarise(mean = mean(count),
            min = min(count),
            max = max(count),
            median = median(count)) %>%
  arrange(desc(max)) %>%
  head(n = 100)

evening_rush %>%
  filter(start_year == "2018") %>% 
  ggplot(aes(x = from_station_id,
             y = to_station_id)) + 
  geom_point(aes(color = bike_id)) +
  facet_wrap(~day)





# congestion_zone %>%
#   addAwesomeMarkers(data = morning_rush,
#                     lat = morning_rush$from_latitude,
#                     lng = morning_rush$from_longitude,
#                     icon = bicycle_blue)


chicago <- getbb("chicago") %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway",
                            "primary",
                            "secondary")) %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = chicago$osm_lines) +
  geom_sf(data = congestion_regions$box,
          color = "red")
