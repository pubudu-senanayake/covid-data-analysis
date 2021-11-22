library(tidyverse)
library(ggplot2)

# Create a people frame, with vax status and id ---------------------------
population <- 10000

personid <- 1:population
vax_rate <- 0.80
vax_protect <- 0.95 # Assumed rate of leaky protection against infection

people <- 
  tibble(personid) %>% 
  mutate(vax_status = rbinom(n = length(personid),
                             size = 1,
                             prob = vax_rate),
         vax_eff = ifelse(vax_status == 1, vax_protect, 0)) %>% 
  mutate(inf_status = 0,
         rec_status = 0,
         inf_ever = 0,
         exposures = 0,
         day_inf = 0)

unvax_id <- people %>% filter(vax_status == 0) %>% select(personid)


# Assign infection parameters ---------------------------------------------

prob_inf_if_exposed <- 0.99
days_recover <- 10

# Simulation set up -------------------------------------------------------

number_of_days <- 100 # How long we test for
days <- seq(1:number_of_days)

disease_summary <- tibble()
days_summary <- tibble()

# Set up initial conditions -----------------------------------------------

seed_cases <- 1 # Initial number to mimic circulation among un-vaccinated

people_infected <- # This is a temporary status holder for initial infections
  people %>%  
  mutate(inf_status = ifelse(vax_status == 0 & # cases among unvaccinated people
                               personid %in% sample(unvax_id$personid, seed_cases),
                             1,
                             inf_status),
         inf_ever = ifelse(inf_ever == 0 & inf_status == 1,
                           1,
                           inf_ever)
         )
                     
# Create a set of exposure scenarios --------------------------------------

# Simulation loop ---------------------------------------------------------

for (day in days) {
  prob_exposure <- sum(people_infected$inf_status)/population
  select_exposed_number <- round(prob_exposure*population)
  
  exposed <- 
    people_infected %>% 
    filter(inf_status == 0 & inf_ever == 0) %>% 
    select(personid)
  
  number_exposed <- length(exposed$personid)
  
  iS_exposed <- sample(people$personid, min(select_exposed_number, number_exposed))
  
  people_infected <-
    people_infected %>% 
    mutate(exposures = ifelse(inf_ever == 0 & personid %in% iS_exposed,
                              exposures + 1,
                              exposures)) %>% 
    mutate(inf_status = ifelse(inf_status == 0 & inf_ever == 0 & personid %in% iS_exposed,
                               rbinom(n = length(iS_exposed),
                                      size = 1,
                                      prob = prob_inf_if_exposed*(1 - vax_eff)),
                               inf_status),
           inf_ever = ifelse(inf_ever == 0 & inf_status == 1,
                             1,
                             inf_ever),
           day_inf = ifelse(day_inf == 0 & inf_status == 1, day, day_inf),
           rec_status = ifelse(inf_status == 1 & day - day_inf > days_recover,
                               1,
                               rec_status),
           )
  
  
  
  # Make summary stats
  
  days_summary <-
    people_infected %>% 
    group_by(vax_status) %>% 
    summarise(infected = sum(inf_status),
              uninfected = n() - sum(inf_status),
              recovered = sum(rec_status),
              exposures = sum(exposures)) %>% 
    mutate(days_num = day) %>% 
    mutate(vax_status = ifelse(vax_status == 1, "yes", "no"))
  
  disease_summary <-
    disease_summary %>% 
    bind_rows(days_summary) %>% 
    select(days_num, vax_status, infected, uninfected, recovered)
}


summary_plot <-
  disease_summary %>% 
  group_by(vax_status) %>% 
  mutate(prop_infected = infected/(infected + uninfected)) %>% 
  ggplot(aes(x = days_num, y = prop_infected, fill = vax_status)) +
  geom_col(position = "dodge")

summary_plot

exposure_plot <-
  people_infected %>% 
  count(vax_status, exposures, name = "n") %>% 
  group_by(vax_status) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = exposures, y = prop, fill = as.character(vax_status))) +
  geom_col(position = "dodge")

exposure_plot


sum(rbinom(n = 500,
       size = 1,
       prob = prob_inf_if_exposed*(1 - 0.8)))


