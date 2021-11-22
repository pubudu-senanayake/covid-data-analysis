library(readr)
library(tidyverse)

# Load Air NZ domestic data -----------------------------------------------

dom_raw <- read_csv("data_conf/dom_travel.csv")

dom_select <-
  dom_raw %>% 
  select(date,
         origin = `From Airport Code`,
         dest = `To Airport Code`,
         pax_count = `PAX Count`)

select_dest <- c("WLG", "NSN", "CHC", "ZQN", "DUD")

dom_akl <-
  dom_select %>% 
  filter(origin == "AKL") %>% 
  filter(dest %in% select_dest) %>% 
  mutate(season = ifelse(date >= "2021-07-11" & date < "2021-08-11", "winter", NA),
         season = ifelse(date >= "2020-12-11" & date < "2021-01-11", "summer", season)) %>% 
  filter(season %in% c("summer", "winter")) %>% 
  count(season, origin, dest, wt = pax_count, name = "pax_count") %>% 
  pivot_wider(names_from = season, values_from = pax_count) %>% 
  mutate(change = round(100*summer/winter, 1))
