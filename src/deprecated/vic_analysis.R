library(readr)
library(tidyverse)
library(ggplot2)
library(zoo)
# Load VIC data -----------------------------------------------------------

vic_raw <- read_csv("data/vic_cases_hosp.csv")

vic_smoothed <-
  vic_raw %>% 
  mutate(days = date - min(date)) %>% 
  mutate(smoothed_cases = rollmean(new_cases, 7, fill = -1, align = "left")) %>% 
  filter(smoothed_cases != -1) %>% 
  filter(date > "2021-08-05" & date < "2021-10-08") %>% 
  select(days, everything())

vic_log <-
  vic_smoothed %>% 
  mutate(log_cases = log(new_cases),
         log_smooth = log(smoothed_cases),
         log_hosp = log(hosp))

ggplot(vic_smoothed) +
  geom_point(aes(x = date, y = new_cases), colour = "blue") +
  geom_line(aes(x = date, y = smoothed_cases)) +
  geom_line(aes(x = date, y = hosp), colour = "red")

ggplot(vic_log) +
  geom_point(aes(x = date, y = log_smooth), colour = "blue") +
  geom_line(aes(x = date, y = log_hosp), colour = "red")

case_fit_hosp <- lm(vic_log$log_hosp ~ as.numeric(vic_log$days))
case_fit_smooth <- lm(vic_log$log_smooth ~ as.numeric(vic_log$days))

coef(case_fit_hosp)
coef(case_fit_smooth)

vic_model_hosp <-
  vic_log %>% 
  mutate(model = exp(case_fit_hosp$coefficients[1])*exp(case_fit_hosp$coefficients[2]*as.numeric(days)))




ggplot(vic_model_hosp) +
  geom_point(aes(x = date, y = hosp), colour = "blue") +
  geom_line(aes(x = date, y = model), colour = "red")

t <- min(vic_model$days):max(vic_model$days)
y <- case_fit$coefficients[1]*exp(case_fit$coefficients[2]*t)
max(y)                                  
