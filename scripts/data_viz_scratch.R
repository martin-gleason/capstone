

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


