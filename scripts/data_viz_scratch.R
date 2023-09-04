library(tidyverse)

divvy_2018 <- readRDS(here::here("inputs", "divvy_2018_data.RDS"))

divvy_growth_projections <- divvy_2018 %>% 
  select(1:3, "user_type") %>%
  mutate(month = month(start_time, label = TRUE),
         year = year(start_time),
         month_year = paste(month, 
                            year,
                            sep = "-")
  ) %>%
  filter(user_type != "Dependent") %>%
  select(-2, -3) %>%
  group_by(month, year, user_type) %>%
  summarize(count = n()) %>%
  select(year, month, user_type, count
  ) %>%
  ungroup()


divvy_growth_projections_wide <- divvy_growth_projections %>%
  pivot_wider(names_from = c(year), values_from = c(count))


# divvy 2018 --------------------------------------------------------------

divvy_2018_plot <-  divvy_2018 %>%
  arrange(start_time) %>%
  mutate(month = month(start_time, label = TRUE),
         year = year(start_time),
         month_year = paste(month, 
                            year,
                            sep = "-")) %>%
  filter(user_type != "Dependent") %>%
  ggplot(aes(x = fct_rev(month), fill = user_type)) +
  geom_bar(stat = "count") +
  facet_wrap(~year) + 
  labs(title = "Rides per month 2018 - 2019",
       x = "Month",
       fill = "Customer Type") +
  theme_minimal() + 
  coord_flip()



# new label ---------------------------------------------------------------


divvy_growth_projections %>%
  ggplot(aes(x = month, y = count, color = user_type, group = user_type)) +
  geom_line() +
  labs(title = "Divvy Usership 2018 - 2019",
       x = "Month",
       y = "Number of Rides",
       color = "Customer Type") +
  facet_wrap(~year) +
  theme_minimal()
