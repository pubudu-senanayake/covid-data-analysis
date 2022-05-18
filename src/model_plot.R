library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

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