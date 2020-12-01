library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

covid_death_raw <- 
  read_csv("data/time_series_covid19_deaths_global.csv")

covid_death_swe <-
  covid_death_raw %>% 
  filter(`Country/Region` == 'Sweden') %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  gather(key = 'dates', value = 'deaths', -`Country/Region`) %>% 
  mutate(dates = as.Date(format(as.Date(dates, '%m/%d/%y'),'%Y-%m-%d'))) %>% 
  mutate(daily_deaths = deaths - lag(deaths))

nov_death_swe <-
  covid_death_swe %>% 
  filter(dates >= as.Date('2020-11-08') & dates <= as.Date('2020-11-28'))

ggplot(nov_death_swe, aes(x = dates, weight = daily_deaths)) +
  geom_bar()

print(sum(nov_death_swe$daily_deaths))
