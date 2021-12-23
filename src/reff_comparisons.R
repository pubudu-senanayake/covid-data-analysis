
# BPM adjustments for international comparisons ---------------------------

bpm_vals <-
  bpm_raw %>% 
  mutate(full_per_nz = 100*pEligImm2) %>% 
  select(date, full_per_nz, Reff_1:Reff_5) %>% 
  filter(date > '2021-10-13' & date < '2021-12-03') ## To take into account conditioning

reff_bpm_plot <-
  bpm_vals %>% 
  ggplot() +
  geom_line(aes(x = date, y = Reff_3), colour = "white", size = 1) +
  geom_ribbon(aes(x = date, ymin = Reff_1, ymax = Reff_5), fill = "grey", alpha = 0.5) +
  theme_dark() +
  labs(title = sprintf("R_eff change in TPM's BPM model prior to CPF"),
       x = "Date",
       y = "Modelled R_eff",
       fill = "Range of modelled R_eff for NZ",
       colour = sprintf("Observed R_eff in %s", state_name)
  )


reff_bpm_plot
ggsave(filename = "plots/r_eff_time_bpm.png", plot = reff_bpm_plot)

starting_pt_vac_rate <- bpm_vals$full_per_nz[1] # Starting point fully vax rate for the model
end_pt_vac_rate <- bpm_vals$full_per_nz[nrow(bpm_vals)] # Starting point fully vax rate for the model


reff_vac_plots <-
  bpm_vals %>% 
  ggplot() +
  geom_ribbon(aes(x = full_per_nz, ymin = Reff_1, ymax = Reff_5), fill = "red", alpha = 0.5) +
  geom_line(data = vic_vac_reff %>% 
              filter(full_per >= starting_pt_vac_rate & full_per <= end_pt_vac_rate),
            aes(x = full_per, y = reff), size = 1, colour = "pink") +
  theme_dark() +
  labs(title = sprintf("Comparison of R_eff decreases between TPM's BPM model and %s", state_name),
       x = "Percentage of eligible (12+) population fully vaccinated",
       y = "R_eff",
       fill = "Range of modelled R_eff for NZ",
       colour = sprintf("Observed R_eff in %s", state_name)
  )

reff_vac_plots
ggsave(filename = "plots/r_eff_compare_bpm_vic.png", plot = reff_vac_plots)


# Normalize R_eff to look at the change in R_eff ----------------------

bpm_norm <-
  bpm_vals %>% 
  mutate(across(Reff_1:Reff_5, ~./.[1]))

vic_reff_norm <- 
  vic_vac_reff %>% 
  filter(full_per >= starting_pt_vac_rate & full_per <= end_pt_vac_rate) %>% 
  mutate(reff = reff/reff[1])

reff_norm_plots <-
  bpm_norm %>% 
  ggplot() +
  geom_ribbon(aes(x = full_per_nz, ymin = Reff_1, ymax = Reff_5), alpha = 0.5, fill = "blue") +
  geom_line(data = vic_reff_norm, aes(x = full_per, y = reff), size = 1.0, colour = "cyan") +
  theme_dark() +
  labs(title = sprintf("Comparison of changes in R_eff between TPM's BPM model and %s", state_name),
       x = "Percentage of eligible (12+) population fully vaccinated",
       y = "R_eff change index (starting values normalized to 1",
       fill = "Range of modelled R_eff for NZ",
       colour = sprintf("Observed R_eff in %s", state_name)
  )


reff_norm_plots
ggsave(filename = "plots/r_eff_normalized_bpm_vic.png", plot = reff_norm_plots)


# Plot Victoria case trajectory for reference -----------------------------

vic_cases_plot <- 
  vic_full_data %>% 
  filter(full_per >= starting_pt_vac_rate & full_per <= end_pt_vac_rate) %>% 
  ggplot() +
  geom_col(aes(x = date, y = new_cases), fill = "gray") +
  geom_line(aes(x = date, y = smoothed_cases), size = 1, colour = "white") +
  theme_dark() +
  labs(title = sprintf("Observed case trajectory in %s", state_name),
       x = "Date",
       y = "New daily cases"
  )

vic_cases_plot
ggsave(filename = "plots/vic_case_trajectory.png", plot = vic_cases_plot)
