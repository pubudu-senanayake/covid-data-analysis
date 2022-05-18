## Fit to Australian data

# Fit exponential curves to Aussie data ------------------------------------------

## Date parameters

start_date <- as.Date("2021-12-01")
end_date <- as.Date("2022-01-21")

date_periods <- seq(from = start_date, to = end_date, by = 7)
fit_window <- 8
coef_results <- tibble()
state_to_fit <- "qld"

## Curve generation parameters
seed_cases <- 20
growth_curve <- tibble()

for (fit_day in date_periods[-length(date_periods)]) {
  
  # Compute fit start and end days
  fit_start_date <- as.Date(fit_day)
  fit_end_date <- as.Date(fit_day) + days(fit_window)
  
  aus_cases <- 
    aus_data %>% 
    filter(state == state_to_fit) %>% 
    filter(date >= as.Date(fit_day) & date < as.Date(fit_day) + days(fit_window)) %>% 
    mutate(days = date - min(date)) %>% 
    select(date, days, new_cases, smooth_cases) %>% 
    mutate(log_cases = log(new_cases),
           log_smooth = log(smooth_cases)
    )
  
  
  case_fit_smooth <- lm(aus_cases$log_smooth ~ as.numeric(aus_cases$days))
  print(as.Date(fit_day))
  
  intercept <- coef(case_fit_smooth)[[1]]
  lambda <- coef(case_fit_smooth)[[2]]
  
  results_temp <- 
    tibble(fit_start_date, fit_end_date, intercept, lambda)
  
  coef_results <- 
    coef_results %>% 
    bind_rows(results_temp)
    
  aus_cases_fit <-
    aus_cases %>%
    mutate(model = exp(case_fit_smooth$coefficients[1])*exp(case_fit_smooth$coefficients[2]*as.numeric(days)))
  
  p <- 
    ggplot(aus_cases_fit) +
    geom_point(aes(x = days, y = smooth_cases)) +
    geom_line(aes(x = days, y = model))
  
  print(p)
  

  # Make growth curve based on seed value -----------------------------------
  

  growth_curve_temp <-
    tibble(date = seq(from = fit_start_date, to = fit_end_date - days(2), by = 1),
                         new_cases = seed_cases) %>%
    mutate(days = date - min(date),
           new_cases = seed_cases*exp(lambda*as.numeric(days+1)))
  
  seed_cases <- max(growth_curve_temp$new_cases)
  
  growth_curve <-
    growth_curve %>% 
    bind_rows(growth_curve_temp)
  
  
}

growth_curve_norm <-
  growth_curve %>% 
  mutate(days = date - min(date)) %>% 
  select(days, new_cases)

ggplot(growth_curve_norm) +
  geom_col(aes(x = days, y = new_cases))

ggplot(aus_cases_fit) +
  geom_point(aes(x = days, y = smooth_cases)) +
  geom_line(aes(x = days, y = model))

ggplot(coef_results) + 
  geom_point(aes(x = 1:7, y = `as.numeric(country_cases$days)`)) +
  labs(x = sprintf("Week since %s", as.Date(AL3_date)  + days(fit_window)),
       y = "Coefficient in single exponential fit")