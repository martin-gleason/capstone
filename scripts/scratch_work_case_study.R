#scratch

divvy_2018 %>% 
  filter(start_time >= min(congestion_2018$time)) %>% 
  arrange(start_time) %>%
  select(start_time) %>%
  lubridate::as_date(start_time)


%>%
  min()

divvy_2018 %>%
  head() %>%
  pull(start_time) |>
  as_date() %>%
  min()


  min()

divvy_2018 %>% 
    filter(start_time >= min(congestion_2018$time) & 
             start_time <= max(congestion_2018$time))
  
divvy_2018$start_time %>% 
  as_date() |>
  min()

divvy_2018$start_time %>% 
  as_date() |>
  max()


congestion_2018$time %>% as_date() %>% min()
congestion_2018$time %>% as_date() %>% max()
  