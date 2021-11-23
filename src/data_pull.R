library(httr)
library(tidyverse)
library(rvest)
library(zoo)
library(ggplot2)

# Check download dates ----------------------------------------------------

download_date <- read_csv("flow_control/download_date.csv")$date


# Pull OWID data ----------------------------------------------------------
if (Sys.Date() > download_date) {
  raw_owid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                            guess_max = 100000)
  
  write_csv(raw_owid_data, "data/owid-covid-data.csv")
  write_csv(tibble(date = Sys.Date()), "flow_control/download_date.csv")
  
} else {
  message("--- Up to date data already downloaded from OWID ---")
}


# Pull Australian case data from COVID-live ------------------------------------

state_list <- c("act", 'qld', "nsw", "vic")
aus_data <- tibble() # initialize empty data frame

for (state in state_list) {
  
  raw_content <- read_html(sprintf("https://covidlive.com.au/report/daily-cases/%s", state))
  raw_tables <- raw_content %>% html_table(fill = TRUE)
  data_raw <- raw_tables[[2]]
  data_cases <- 
    data_raw %>% 
    filter(!(NEW == "-" | NEW == "")) %>% 
    mutate(date = DATE,
           state = state,
           new_cases = as.numeric(gsub(",", "",NEW)),
           total_cases = as.numeric(gsub(",", "",CASES)), 
           net_change = as.numeric(gsub(",", "",NET))) %>% 
    select(date, state, new_cases, total_cases, net_change)
  
  
  # Pull Australian hospital data from COVID-live ---------------------------
  
  raw_content <- read_html(sprintf("https://covidlive.com.au/report/daily-hospitalised/%s", state))
  raw_tables <- raw_content %>% html_table(fill = TRUE)
  data_raw <- raw_tables[[2]]
  data_hosp <- 
    data_raw %>% 
    mutate(date = DATE,
           state = state,
           hosp = as.numeric(gsub(",", "",HOSP)),
           icu = as.numeric(gsub(",", "",ICU)), 
           vent = as.numeric(gsub(",", "",VENT))) %>% 
    select(date, state, hosp, icu, vent)
  
  
  # Combine case and hosp data ----------------------------------------------
  
  aus_data_proc <-
    data_cases %>% 
    left_join(data_hosp, by = c("date","state")) %>% 
    replace_na(list(hosp = 0, icu = 0, vent = 0)) %>% 
    mutate(date = strptime(date, format = "%d %b %y")) %>% 
    filter(date >= "2020-03-01")
  
  aus_data <-
    bind_rows(aus_data, aus_data_proc)
  
}

aus_data <- 
  aus_data %>% 
  mutate(date = as.Date(date),
         as_at = Sys.Date())

write_csv(aus_data, sprintf("data_push/aus_data.csv"))
