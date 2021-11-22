## Read data

initial_detection <- as.Date("2021-08-12")
intervention_date <- as.Date("2021-08-12")
int_type <- "Level 3+ restrictions"
country <- "ACT, Australia"
act_population <- 432000

act_raw <- 
  read_csv("data/act_data.csv") %>% 
  select(-cum_tests)

act_data <-
  act_raw %>% 
  mutate(date = as_date(date, format = "%d-%b-%y")) %>% 
  arrange(date) %>% 
  mutate(smoothed_cases = rollmean(daily_cases, 7, fill = 0, align = "right"),
         smoothed_tests = rollmean(daily_tests, 7, fill = 0, align = "right")) %>% 
  mutate(pos_rate = round(100*daily_cases/daily_tests, digits = 1))

cluster_size <- sum(act_data$daily_cases)


act_case_plot <-
  act_data %>% 
  filter(date > "2021-08-01") %>% 
  select(date, smoothed_cases, daily_cases, community, quarantine, investigated) %>% 
  pivot_longer(community:investigated, names_to = "case_type", values_to = "count") %>% 
  ggplot() +
  geom_col(aes(x = date, y = count, fill = case_type)) +
  geom_line(aes(x = date, y = smoothed_cases), size = 1)+
  annotate(geom = "vline",
           x = c(initial_detection, intervention_date),
           xintercept = c(initial_detection, intervention_date),
           linetype = c("dashed", "solid"),
           colour = c("red", "red"),
           size = 1) +
  annotate(geom = "text",
           label = c("", int_type),
           colour = c("black", "black"),
           x = c(initial_detection - days(0), intervention_date - days(0)),
           y = c(10, 10),
           angle = 90, 
           vjust = 1) +
  scale_x_date(date_breaks = "3 day") +
  scale_fill_manual(values =c("#F8766D", "grey", "#00C094"), labels = c("Infectious in community", "Under investigation", "In quarantine")) +
  labs(
    x = "Date",
    y = "Number of daily cases",
    title = sprintf("%s daily case numbers", country),
    fill = "Case status"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "bottom")

act_case_plot

act_test_plot <-
  act_data %>% 
  filter(date > "2021-08-01") %>% 
  ggplot() +
  geom_col(aes(x = date, y = daily_tests), fill = "#F8766D") +
  geom_line(aes(x = date, y = smoothed_tests)) +
  annotate(geom = "vline",
           x = c(initial_detection, intervention_date),
           xintercept = c(initial_detection, intervention_date),
           linetype = c("dashed", "solid"),
           colour = c("red", "red"),
           size = 1) +
  annotate(geom = "text",
           label = c("", int_type),
           colour = c("black", "black"),
           x = c(initial_detection - days(0), intervention_date - days(0)),
           y = c(4000, 4000),
           angle = 90, 
           vjust = 1) +
  scale_x_date(date_breaks = "3 day") +
  labs(
    x = "Date",
    y = "Number of daily tests",
    title = sprintf("%s daily test numbers", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

act_test_plot

act_test_rate_plot <-
  act_data %>% 
  mutate(test_rate = round(100*daily_tests/act_population, digits = 1),
         smoothed_test_rate = rollmean(test_rate, 7, fill = 0, align = "right")) %>% 
  filter(date > "2021-08-01") %>% 
  ggplot() +
  geom_col(aes(x = date, y = test_rate), fill = "#F8766D") +
  geom_line(aes(x = date, y = smoothed_test_rate)) +
  annotate(geom = "vline",
           x = c(initial_detection, intervention_date),
           xintercept = c(initial_detection, intervention_date),
           linetype = c("dashed", "solid"),
           colour = c("red", "red"),
           size = 1) +
  annotate(geom = "text",
           label = c("", int_type),
           colour = c("black", "black"),
           x = c(initial_detection - days(0), intervention_date - days(2)),
           y = c(1.0, 1.0),
           angle = 90, 
           vjust = 1) +
  scale_x_date(date_breaks = "3 day") +
  labs(
    x = "Date",
    y = "Fraction of daily tests (%)",
    title = sprintf("%s daily tests as a fraction of population", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

act_test_rate_plot

act_pos_rate_plot <-
  act_data %>% 
  filter(date > "2021-08-01") %>% 
  ggplot() +
  geom_col(aes(x = date, y = pos_rate), fill = "#F8766D") +
  annotate(geom = "vline",
           x = c(initial_detection, intervention_date),
           xintercept = c(initial_detection, intervention_date),
           linetype = c("dashed", "solid"),
           colour = c("red", "red"),
           size = 1) +
  annotate(geom = "text",
           label = c("", int_type),
           colour = c("black", "black"),
           x = c(initial_detection - days(0), intervention_date - days(2)),
           y = c(1.0, 1.0),
           angle = 90, 
           vjust = 1) +
  scale_x_date(date_breaks = "3 day") +
  labs(
    x = "Date",
    y = "Positive rate of daily tests (%)",
    title = sprintf("%s daily tests as a fraction of population", country)
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

act_pos_rate_plot


