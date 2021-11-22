
# Vaccination data analysis -----------------------------------------------

vax_data_raw <- read_csv("data/covid_vaccinations_21_09_2021.csv")

vax_data <-
  vax_data_raw %>% 
  select(age = `Age group`, 
         gender = Gender, 
         dose_one = `First dose administered`, 
         dose_two = `Second dose administered`,
         population = Population) %>% 
  pivot_longer(dose_one:population, names_to = "type", values_to = "count") %>% 
  count(age, type, wt = count, name = "count") %>% 
  filter(age != "Various") %>% 
  pivot_wider(names_from = type, values_from = count) %>% 
  mutate(dose_one_prop = dose_one/population,
         dose_two_prop = dose_two/population)
