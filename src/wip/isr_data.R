library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)


## Israel analysis


# Read data from OWID -----------------------------------------------------

isr_raw <- 
  read_csv("data/isr_data.csv", col_names = FALSE) %>% 
  t() %>% as.data.frame() %>% 
  row_to_names(row_number = 1)
  
isr_shaped <-
  isr_raw %>% 
  select(day, stringency, case_num) %>% 
  mutate(day = as_date(day, format = "%d%b%Y"),
         stringency = as.numeric(stringency),
         case_num = as.numeric(case_num)) %>% 
  mutate(new_cases = case_num - lag(case_num))

isr_plot <-
  isr_shaped %>% 
  filter(day > as_date("2021-02-09")) %>% 
  ggplot() +
  geom_line(aes(x = day, y = new_cases), colour = "red") +
  geom_line(aes(x = day, y = 60*stringency)) +
  scale_y_continuous(name = "New cases", 
                     sec.axis = sec_axis(~./60, name = "Strigency index (0-100)")
                     ) +
  labs(
    x = "Date (2021)",
    title = "Israel case numbers compared with Strigency index"
  )

  
isr_plot
  
