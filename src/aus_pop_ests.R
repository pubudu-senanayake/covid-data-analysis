## Aus pop estimates

aus_pop_raw <- 
  read_csv("data/ABS_aus_pop_age_reg.csv")

aus_pop_grps <-
  read_csv("data/ABS_aus_pop_age_grp.csv") %>% 
  select(Region, Age, Count) %>% 
  filter(!is.na(Count))


# Age parameter for selection

age_old <- 65

aus_pop_summary <-
  aus_pop_raw %>% 
  mutate(over_age = ifelse(Age >= age_old, sprintf("%s+",age_old), sprintf("%s-",age_old))) %>% 
  group_by(Region, over_age) %>% 
  mutate(count_age_group = sum(Count)) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  mutate(prop_age_group = count_age_group/sum(count_age_group))

aus_pop_groups <-
  aus_pop_grps %>% 
  group_by(Region) %>% 
  mutate(props = round(100*Count/sum(Count), digits = 1)) %>% 
  ungroup()

age_plot <-
  aus_pop_groups %>% 
  ggplot() +
  geom_col(aes(x = Age, y = props)) +
  facet_wrap(~Region) +
  coord_flip() +
  labs(x = "Age group",
       y = "Proportion in each age group (%)"
       )
  
age_plot

write_csv(aus_pop_groups, "out/aus_age_group_pops.csv")
