## Global dataset analysis from OWID

# Read in data ------------------------------------------------------------

global_data_raw <-
  read_csv("data/owid-covid-data.csv", guess_max = 250000)

country <- "Singapore"
initial_detection <- as.Date("2021-04-20")
intervention_date <- as.Date("2021-05-19")
int_type <- "Level 3 restrictions"

country_data_raw <-
  global_data_raw %>% 
  filter(location == country) %>% 
  #filter(iso_code == "") %>% 
  select(iso_code, location, date, new_cases, new_cases_smoothed, reproduction_rate, 
         stringency_index, people_vaccinated_per_hundred, new_tests, new_tests_smoothed,
         positive_rate) %>% 
  mutate(R_string = reproduction_rate*stringency_index) %>% 
  mutate(change_R = c(NA, diff(reproduction_rate))) %>% 
  replace_na(list(new_cases = 0,
                  new_cases_smoothed = 0,
                  reproduction_rate = 1,
                  stringency_index = 0,
                  people_vaccinated_per_hundred = 0,
                  positive_rate = 0,
                  new_tests = 0,
                  new_tests_smoothed = 0,
                  change_R = 0)) %>% 
  mutate(smoothed_cases = rollmean(new_cases, 7, fill = 0, align = "right"),
         smoothed_tests = rollmean(new_cases, 7, fill = 0, align = "right"))


nzr_data_raw <-
  global_data_raw %>% 
  filter(iso_code == "NZL") %>% 
  select(iso_code, location, date, new_cases, new_cases_smoothed, reproduction_rate, stringency_index, people_vaccinated_per_hundred) %>% 
  mutate(R_string = reproduction_rate*stringency_index) %>% 
  na.omit()


country_plots_r_eff <-
  country_data_raw %>% 
  filter(date >= "2021-08-01" & date < "2021-11-11") %>% 
  ggplot() +
  #geom_line(aes(x = date, y = reproduction_rate) , colour = "red") +
  #geom_line(aes(x = date, y = stringency_index*400), colour = "blue") +
  geom_line(aes(x = date, y = smoothed_cases), colour = "green", size = 1.0) +
  #geom_line(aes(x = date, y = 1), colour = "black"Y) +
  #geom_line(aes(x = date, y = c(1, diff(stringency_index/max(stringency_index)) + 1)), colour = "black") +
  #geom_line(aes(x = date, y = c(NA,diff(reproduction_rate))), colour = "cyan")
  # scale_y_continuous(name = "Reproduction rate", 
  #                    sec.axis = sec_axis(~./400, name = "Strigency index/Vaccination rate (0-100)")
  # ) +
  labs(
    x = "Date",
    title = sprintf("%s R_eff compared with Strigency index", country)
  )

country_plots_r_eff

country_plots_cases <-
  country_data_raw %>% 
  filter(date >= "2021-07-20" & date < "2021-08-19") %>% 
  ggplot() +
  geom_col(aes(x = date, y = new_cases), fill = "#F8766D", size = 0.5) +
  geom_line(aes(x = date, y = new_cases_smoothed), colour = "black", size = 1.0) +
  # annotate(geom = "vline",
  #          x = c(initial_detection, intervention_date),
  #          xintercept = c(initial_detection, intervention_date),
  #          linetype = c("dashed", "solid"),
  #          colour = c("green", "green"),
  #          size = 1) +
  # annotate(geom = "text",
  #          label = c("Detection", int_type),
  #          colour = c("black", "white"),
  #          x = c(initial_detection - days(5), intervention_date + days(0)),
  #          y = c(50, 50),
#          angle = 90, 
#          vjust = 1) +
scale_x_date(date_breaks = "7 day") +
  scale_y_log10(name = "Case numbers (log scale)") +
  labs(
    x = "Date",
    title = sprintf("%s daily case numbers", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

country_plots_cases

country_plots_restrict <-
  country_data_raw %>% 
  filter(date >= "2021-07-20" & date < "2021-08-19") %>% 
  ggplot() +
  geom_col(aes(x = date, y = new_cases), fill = "#F8766D", size = 0.5) +
  geom_line(aes(x = date, y = new_cases_smoothed), colour = "black", size = 1.0) +
  #geom_line(aes(x = date, y = stringency_index*1.5), colour = "blue") +
  # annotate(geom = "vline",
  #          x = c(initial_detection, intervention_date),
  #          xintercept = c(initial_detection, intervention_date),
  #          linetype = c("dashed", "solid"),
  #          colour = c("green", "green"),
  #          size = 1) +
  # annotate(geom = "text",
  #          label = c("Detection", int_type),
  #          colour = c("black", "black"),
  #          x = c(initial_detection - days(5), intervention_date - days(5)),
#          y = c(600, 600),
#          angle = 90, 
#          vjust = 1) +
scale_x_date(date_breaks = "7 day") +
  # scale_y_continuous(name = "Case numbers", 
  #                    sec.axis = sec_axis(~./1.5, name = "Strigency index (0 - 100)")
  # ) +
  labs(
    x = "Date",
    y = "Case numbers",
    title = sprintf("%s daily case numbers", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

country_plots_restrict

country_plots_pos_rate <-
  country_data_raw %>% 
  filter(date >= "2021-07-20" & date < "2021-08-19") %>% 
  ggplot() +
  geom_line(aes(x = date, y = new_cases), colour = "red", size = 0.5) +
  geom_line(aes(x = date, y = new_cases_smoothed), colour = "black", size = 1.0) +
  geom_line(aes(x = date, y = stringency_index*7), colour = "blue") +
  geom_area(aes(x = date, y = positive_rate*700), fill = "green", size = 0.5) +
  scale_x_date(date_breaks = "7 day") +
  scale_y_continuous(name = "Case numbers", 
                     sec.axis = sec_axis(~./7, name = "Strigency index (0 - 100)")
  ) +
  labs(
    x = "Date",
    title = sprintf("%s daily case numbers and intervention stringency", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

country_plots_pos_rate

country_plots_tests <-
  country_data_raw %>% 
  filter(date >= "2021-04-01" & date < "2021-08-19") %>% 
  ggplot() +
  geom_col(aes(x = date, y = new_tests), fill = "#00C094", size = 0.5) +
  geom_line(aes(x = date, y = new_tests_smoothed), colour = "black", size = 1.0) +
  geom_area(aes(x = date, y = positive_rate*300000), fill = "#F8766D", size = 0.5, alpha = 0.7) +
  annotate(geom = "vline",
           x = c(initial_detection, intervention_date),
           xintercept = c(initial_detection, intervention_date),
           linetype = c("dashed", "solid"),
           colour = c("red", "red"),
           size = 1) +
  annotate(geom = "text",
           label = c("Detection", int_type),
           colour = c("black", "black"),
           x = c(initial_detection - days(0), intervention_date - days(0)),
           y = c(25000, 25000),
           angle = 90, 
           vjust = 1) +
  scale_x_date(date_breaks = "7 day") +
  scale_y_continuous(name = "Number of tests", 
                     sec.axis = sec_axis(~./3000, name = "Positivity rate (%)")
  ) +
  labs(
    x = "Date",
    title = sprintf("%s daily test numbers", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

country_plots_tests




isr_corr_plot <-
  isr_data_raw %>% 
  filter(date >= "2020-04-01" & date < "2021-06-16") %>% 
  ggplot() +
  geom_point(aes(x = stringency_index, y = reproduction_rate) , colour = "red") +
  geom_point(aes(x = people_vaccinated_per_hundred, y = reproduction_rate) , colour = "green")

isr_corr_plot
