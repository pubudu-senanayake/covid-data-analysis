library(tidyverse)
library(ggplot2)

percent_vax <- seq(0,1,0.01)
vax_eff <- c(0, 0.5, 0.8, 0.9, 0.95, 1.00)

vax_curve <- expand_grid(percent_vax, vax_eff) %>% 
  mutate(percent_vax_cases = (percent_vax-(percent_vax*vax_eff))/(1-(percent_vax*vax_eff)))

#PVC = (PPV-(PPV*VE))/(1-(PPV*VE))

ggplot(vax_curve) +
  geom_line(aes(x = percent_vax, y = percent_vax_cases, colour = as.character(vax_eff))) +
  labs(x = "Fraction of population vaccinated",
       y = "Fraction of vaccinated people among given outcomes",
       colour = "Assumed vaccine effectiveness against given outcome") +
  theme(legend.position = "bottom",
        aspect.ratio = 1)
