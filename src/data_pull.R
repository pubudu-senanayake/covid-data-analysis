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


# Pull Australian data from COVID-live ------------------------------------

state_list <- c("act", 'qld', "nsw", "vic")
aus_data <- tibble() # initialize empty data frames
aus_vac_data <- tibble()
#state_list <- "vic" ## Use this for testing a single state in the loop below

for (state in state_list) {
  

  # Pull case data from COVID-live ------------------------------------------

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
  
  # Pull vaccination data from COVID-live -----------------------------------

  raw_content <- read_html(sprintf("https://covidlive.com.au/report/daily-vaccinations-people/%s", state))
  raw_tables <- raw_content %>% html_table(fill = TRUE)
  data_raw <- raw_tables[[2]]
  
  data_vac <- 
    data_raw %>% 
    mutate(date = DATE,
           state = state,
           second_doses = as.numeric(gsub(",", "",SECOND)),
           full_per = as.numeric(gsub("%","",`12+`)), 
           change = as.numeric(NET)) %>% 
    select(date, state, second_doses, full_per, change) %>% 
    mutate(date = strptime(date, format = "%d %b %y"))
  
  aus_vac_data <- 
    bind_rows(aus_vac_data, data_vac)
  
}

aus_data <- 
  aus_data %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(active_cases = rollapply(data = new_cases, width = 14, sum, fill = NA, align = "right")) %>% 
  mutate(hosp_ratio = hosp/active_cases) %>% 
  ungroup() %>% 
  mutate(date = as.Date(date),
         as_at = Sys.Date())

aus_vac_data <-
  aus_vac_data %>% 
  mutate(date = as.Date(date),
         as_at = Sys.Date()) %>% 
  arrange(date)


write_csv(aus_data, sprintf("data_push/aus_data.csv"))
write_csv(aus_vac_data, sprintf("data_push/aus_vac_data.csv"))



# Pull in BPM data --------------------------------------------------------

bpm_raw <- readxl::read_xlsx("data/BPM_results_trafficlight_28-Nov-2021_with_active_cases.xlsx")


# Pull in r_eff data for Victoria -----------------------------------------

vic_reff_raw <- read_csv("data/vic_reff.csv") ## data downloaded from https://www.covid19data.com.au/
