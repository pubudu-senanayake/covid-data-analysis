## Ingest public modelling data

bpm_raw_delta <- readxl::read_excel("data/scenario-results-with-CT-capacity-limit.xlsx")

bpm_delta <- 
  bpm_raw_delta %>% 
  select(Scenario, weekStarting, nCases_1:nCases_3)
  

nz_cases_raw <-
  read_csv("data/nz_overview_case_curve__202202110152.csv") %>% 
  filter(!is.na(Date)) %>% 
  mutate(Date = dmy(Date))

nz_cases <- 
  nz_cases_raw %>% 
  count(Date, wt = `Daily confirmed`, name = "new_cases") %>% 
  mutate(weekly_cases = rollapply(data = new_cases, width = 7, sum, fill = NA, align = "left"))

compare_model <-
  bpm_delta %>% 
  inner_join(nz_cases, by = c("weekStarting" = "Date")) %>% 
  select(-new_cases) %>% 
  filter(!is.na(weekly_cases)) %>% 
  filter(Scenario %in% c("Medium-transmission", "High-transmission"))

write_csv(compare_model, "out/compare_model.csv")

compare_model_data <- read_csv("out/compare_model.csv")

model_plot <-
  compare_model_data %>% 
  ggplot() +
  geom_ribbon(aes(x = weekStarting, ymin = nCases_1, ymax = nCases_3, fill = Scenario), alpha = 0.5) +
  geom_line(aes(x = weekStarting, y = nCases_2, colour = Scenario), size = 1.0) +
  scale_colour_manual(values = c("#ffdede", "#bdf0fa")) +
  geom_point(aes(x = weekStarting, y = weekly_cases)) +
  geom_vline(xintercept = as.POSIXct(as.Date("2021-10-16"))) +
  scale_x_datetime(date_breaks = "2 weeks",
                   limits = c(as.POSIXct(as.Date("2021-10-06")), NA)) +
  labs(x = "Week starting",
       y = "Number of weekly cases") +
  theme(legend.position = "bottom")

model_plot
