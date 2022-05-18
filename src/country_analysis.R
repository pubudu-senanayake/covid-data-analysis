library(tidyverse)
library(zoo)
library(lubridate)
library(ggplot2)


## Global dataset analysis from OWID

# Read in data ------------------------------------------------------------

global_data_raw <-
  read_csv("data/owid-covid-data.csv", guess_max = 200000)

country <- "New Zealand"
initial_detection <- as.Date("2021-04-20")
intervention_date <- as.Date("2021-05-19")
int_type <- "Level 3 restrictions"

country_data_raw <-
  global_data_raw %>% 
  filter(location == country) %>% 
  #filter(iso_code == "") %>% 
  select(iso_code, location, date, new_cases, new_cases_smoothed, hosp_patients, reproduction_rate, 
         stringency_index, people_vaccinated_per_hundred, new_tests, new_tests_smoothed,
         positive_rate) %>% 
  mutate(R_string = reproduction_rate*stringency_index) %>% 
  mutate(change_R = c(NA, diff(reproduction_rate))) %>% 
  replace_na(list(new_cases = 0,
                  new_cases_smoothed = 0,
                  hosp_patients = 0,
                  reproduction_rate = 1,
                  stringency_index = 0,
                  people_vaccinated_per_hundred = 0,
                  positive_rate = 0,
                  new_tests = 0,
                  new_tests_smoothed = 0,
                  change_R = 0)) %>% 
  mutate(smoothed_cases = rollmean(new_cases, 3, fill = 0, align = "right"),
         smoothed_tests = rollmean(new_cases, 3, fill = 0, align = "right"))
  

nzr_data_raw <-
  global_data_raw %>% 
  filter(iso_code == "NZL") %>% 
  select(iso_code, location, date, new_cases, new_cases_smoothed, reproduction_rate, stringency_index, people_vaccinated_per_hundred) %>% 
  mutate(R_string = reproduction_rate*stringency_index) %>% 
  na.omit()


country_plots_r_eff <-
  country_data_raw %>% 
  filter(date >= "2021-01-01" & date < "2021-11-10") %>% 
  ggplot() +
  #geom_line(aes(x = date, y = reproduction_rate) , colour = "red") +
  geom_line(aes(x = date, y = new_tests_smoothed), colour = "blue") +
  geom_line(aes(x = date, y = new_cases), colour = "green", size = 1.0) +
  #geom_line(aes(x = date, y = 1), colour = "black") +
  #geom_line(aes(x = date, y = c(1, diff(stringency_index/max(stringency_index)) + 1)), colour = "black") +
  #geom_line(aes(x = date, y = c(NA,diff(reproduction_rate))), colour = "cyan")
  # scale_y_continuous(name = "Reproduction rate", 
  #                    sec.axis = sec_axis(~./400, name = "Strigency index/Vaccination rate (0-100)")
  # ) +
  labs(
    x = "Date",
    title = sprintf("%s cases compared with testing", country)
  )

country_plots_r_eff


# Fit exponential curves to countries' data ------------------------------------------

AL3_date <- "2021-09-27"

date_periods <- seq(from = as.Date("2021-09-27"), to = as.Date("2021-11-18"), by = 7)
fit_window <- 21
coef_results <- tibble()

for (day in date_periods[-length(date_periods)]) {
  
  country_cases <- 
    country_data_raw %>% 
    filter(date >= as.Date(day) & date < as.Date(day) + days(fit_window)) %>% 
    mutate(days = date - min(date)) %>% 
    select(iso_code, date, days, new_cases, smoothed_cases, hosp = hosp_patients) %>% 
    mutate(log_cases = log(new_cases),
           log_smooth = log(smoothed_cases),
           log_hosp = log(hosp))
  
  
  
  case_fit_smooth <- lm(country_cases$log_smooth ~ as.numeric(country_cases$days))
  print(day)
  coef_results <- coef_results %>% bind_rows(coef(case_fit_smooth))
  
  
  country_cases_fit <-
    country_cases %>%
    mutate(model = exp(case_fit_smooth$coefficients[1])*exp(case_fit_smooth$coefficients[2]*as.numeric(days)))
  
  p <- 
    ggplot(country_cases_fit) +
    geom_point(aes(x = days, y = smoothed_cases)) +
    geom_line(aes(x = days, y = model))
  
  print(p)
  
}

ggplot(country_cases_fit) +
  geom_point(aes(x = days, y = smoothed_cases)) +
  geom_line(aes(x = days, y = model))

ggplot(coef_results) + 
  geom_point(aes(x = 1:7, y = `as.numeric(country_cases$days)`)) +
  labs(x = sprintf("Week since %s", as.Date(AL3_date)  + days(fit_window)),
       y = "Coefficient in single exponential fit")
