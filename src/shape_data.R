
# Shape data for international comparisons --------------------------------

select_state <- "nsw"
state_name <- "NSW"

# Australian data - Vic ---------------------------------------------------

vic_full_data <-
  aus_data %>% 
  arrange(date) %>% 
  left_join(aus_vac_data, by = c("date", "state")) %>% 
  filter(state == select_state,
         date >= "2021-08-01") %>% 
  select(date, state, full_per, new_cases, total_cases, active_cases, hosp, hosp_ratio) %>% 
  mutate(smoothed_cases = rollapply(new_cases, width = 7, FUN = mean, align = "right", fill = NA)) %>% 
  filter(full_per >= starting_pt_vac_rate & full_per <= end_pt_vac_rate)

vic_vac <-
  aus_vac_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(state == select_state)

vic_reff <-
  vic_reff_raw %>% 
  mutate(date = as.Date(Date)) %>% 
  select(date, reff = `Reff (yellow line)`)

vic_vac_reff <-
  vic_vac %>% 
  left_join(vic_reff, by ="date") %>% 
  filter(!is.na(reff))

reff_plot <-
  vic_vac_reff %>% 
  ggplot(aes(x = full_per, y = reff)) +
  geom_line()

reff_plot

