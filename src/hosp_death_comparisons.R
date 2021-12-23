library(matrixStats)
# Hospitalizations and death analysis -------------------------------------

lag_to_hosp <- 10 # Number of days for a case to end up in hospital on average

bpm_outcomes <-
  bpm_raw %>% 
  mutate(full_per_nz = 100*pEligImm2) %>% 
  select(date, full_per_nz, nInfected_1:nDeaths_5) %>% 
  filter(date > '2021-10-13' & date < '2021-12-03') %>% ## To take into account conditioning
  mutate(across(nHospBeds_1:nHospBeds_5,
                ~./get(str_c("nActive", str_sub(cur_column(), nchar(cur_column()) - 1,nchar(cur_column())))),
                       .names = "ratio_{.col}")) %>% 
  mutate(mean_ratio = select(., ratio_nHospBeds_1:ratio_nHospBeds_5) %>% rowMeans(),
         spread = rowSds(as.matrix(select(., ratio_nHospBeds_1:ratio_nHospBeds_5)))
         )

cases_vac_plots <-
  bpm_outcomes %>% 
  ggplot() +
  geom_ribbon(aes(x = full_per_nz, ymin = nCases_1, ymax = nCases_5), alpha = 0.5) +
  geom_line(aes(x = full_per_nz, y = nCases_3), colour = "grey")

cases_vac_plots


# Plot the hospitalization ratios -----------------------------------------


# hosp_ratio_plot <-
#   bpm_outcomes %>% 
#   select(date, full_per_nz, ratio_nHospBeds_1:ratio_nHospBeds_5) %>% 
#   pivot_longer(ratio_nHospBeds_1:ratio_nHospBeds_5, names_to = "trajectory", values_to = "ratio_hosp") %>% 
#   ggplot() +
#   geom_line(aes(x = full_per_nz, y=ratio_hosp, colour = trajectory), alpha = 0.7) +
#   geom_line(data = vic_full_data %>% 
#               filter(full_per >= starting_pt_vac_rate),
#             aes(x = full_per, y = hosp_ratio))

hosp_ratio_plot <-
  bpm_outcomes %>% 
  select(date, full_per_nz, mean_ratio, spread) %>% 
  ggplot() +
  geom_ribbon(aes(x = full_per_nz,
                  ymin = mean_ratio - spread,
                  ymax = mean_ratio + spread),
              alpha = 0.7,
              fill = "#006400") +
  geom_line(data = vic_full_data %>% 
              filter(full_per >= starting_pt_vac_rate),
            aes(x = full_per, y = hosp_ratio), size = 1, colour = "green") +
  theme_dark() +
  labs(title = sprintf("Comparison of hospitalization ratios between TPM's BPM model and %s", state_name),
       x = "Percentage of eligible (12+) population fully vaccinated",
       y = "Ratio of active cases in hospital",
       fill = "Range of modelled R_eff for NZ",
       colour = sprintf("Observed R_eff in %s", state_name)
  )


hosp_ratio_plot    
ggsave(filename = "plots/hosp_ratio_bpm_vic.png", plot = hosp_ratio_plot)  
