
# Plot and analyze Aus data -----------------------------------------------

select_state <- "qld"
state_name <- toupper(select_state)
start_date <- as.Date("2021-12-12")
active_per <- 14 # How long a case is considered active
delay_to_hosp <- 10 # How long from a case being active to hospitalization

cases_plot <-
  aus_data %>% 
  filter(state == select_state,
         date >= start_date) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_col()

cases_plot

tot_cases <-
  aus_data %>% 
  filter(state == select_state,
         date >= start_date) %>% 
  count(wt = new_cases)

print(tot_cases)

tot_cases_plot <-
  aus_data %>% 
  select(date, state, new_cases) %>% 
  group_by(date) %>% 
  count(date, wt = new_cases, name = "new_cases") %>% 
  filter(date >= start_date) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_line()

tot_cases_plot


hosp_levels <-
  aus_data %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(active_cases = rollapply(data = new_cases, width = active_per, sum, fill = 0, align = "right")) %>% 
  mutate(hosp_ratio = hosp/lag(active_cases, delay_to_hosp)) %>% 
  mutate(norm_case = active_cases/max(active_cases),
         norm_hosp = hosp/max(hosp)) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date),
         as_at = Sys.Date()) %>% 
  filter(date >= start_date)

# %>% 
#   filter(state == select_state) %>% 
#   filter(date >= start_date)

hosp_plot <-
  hosp_levels %>% 
  ggplot() +
  geom_line(aes(x = date, y = norm_hosp, colour = "Hospitalisations")) +
  geom_line(aes(x = date, y = norm_case, colour = "Active cases")) +
  #geom_col(aes(x = date, y = hosp_ratio, colour = sprintf("Ratio of hosp to active cases lagged by %s days",delay_to_hosp))) +
  facet_wrap(~state) +
  labs(title = sprintf("Normalized Hospitalisation and case load comparisons", state_name),
       x = "Date",
       y = "Normalized quantity [Y/max(Y)]") +
  theme(
    legend.position = "bottom"
  )

hosp_plot


# Vic_specific_stuff ------------------------------------------------------


# vic_vac <-
#   aus_vac_data %>% 
#   mutate(date = as.Date(date)) %>% 
#   filter(state == select_state)
#   
# vic_reff_raw <- read_csv("data/vic_reff.csv")
# 
# vic_reff <-
#   vic_reff_raw %>% 
#   mutate(date = as.Date(Date)) %>% 
#   select(date, reff = `Reff (yellow line)`)
# 
# vic_vac_reff <-
#   vic_vac %>% 
#   left_join(vic_reff, by ="date") %>% 
#   filter(!is.na(reff))
# 
# reff_plot <-
#   vic_vac_reff %>% 
#   ggplot(aes(x = full_per, y = reff)) +
#   geom_line()
# 
# reff_plot
