
# Plot and analyze Aus data -----------------------------------------------

select_state <- "vic"

cases_plot <-
  aus_data %>% 
  filter(state == select_state) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_line()

cases_plot

vic_vac <-
  aus_vac_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(state == select_state)
  
vic_reff_raw <- read_csv("data/vic_reff.csv")

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
