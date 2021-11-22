## Global dataset analysis from OWID

# Read in data ------------------------------------------------------------

global_data_raw <-
  read_csv("data/owid-covid-data.csv", guess_max = 25000)

isr_data_raw <-
  global_data_raw %>% 
  filter(iso_code == "ISR") %>% 
  select(iso_code, location, date, new_cases, new_cases_smoothed, reproduction_rate, stringency_index, people_vaccinated_per_hundred) %>% 
  mutate(R_string = reproduction_rate*stringency_index) %>% 
  mutate(change_R = c(NA, diff(reproduction_rate))) %>% 
  replace_na(list(new_cases = 0,
                  new_cases_smoothed = 0,
                  reproduction_rate = 1,
                  stringency_index = 0,
                  people_vaccinated_per_hundred = 0,
                  change_R = 0))
  

nzr_data_raw <-
  global_data_raw %>% 
  filter(iso_code == "NZL") %>% 
  select(iso_code, location, date, new_cases, new_cases_smoothed, reproduction_rate, stringency_index, people_vaccinated_per_hundred) %>% 
  mutate(R_string = reproduction_rate*stringency_index) %>% 
  na.omit()


isr_plots <-
  isr_data_raw %>% 
  filter(date >= "2020-04-01" & date < "2021-08-19") %>% 
  ggplot() +
  geom_line(aes(x = date, y = reproduction_rate) , colour = "red") +
  geom_line(aes(x = date, y = stringency_index/50), colour = "blue") +
  geom_line(aes(x = date, y = people_vaccinated_per_hundred/50), colour = "green", size = 1.0) +
  geom_line(aes(x = date, y = 1), colour = "black") +
  geom_line(aes(x = date, y = c(1, diff(stringency_index/max(stringency_index)) + 1)), colour = "black") +
  #geom_line(aes(x = date, y = c(NA,diff(reproduction_rate))), colour = "cyan")
  scale_y_continuous(name = "Reproduction rate", 
                     sec.axis = sec_axis(~.*50, name = "Strigency index/Vaccination rate (0-100)")
  ) +
  labs(
    x = "Date",
    title = "Israel R_eff compared with Strigency index"
  )

isr_plots

isr_corr_plot <-
  isr_data_raw %>% 
  filter(date >= "2020-04-01" & date < "2021-06-16") %>% 
  ggplot() +
  geom_point(aes(x = stringency_index, y = reproduction_rate) , colour = "red") +
  geom_point(aes(x = people_vaccinated_per_hundred, y = reproduction_rate) , colour = "green")

isr_corr_plot
