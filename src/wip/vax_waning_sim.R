library(EpiDynamics)
library(tidyverse)


## Population level protection against a given variant


# Compute vaccine waning effects ------------------------------------------

waning_rate <- 0.03 ## Rate of waning per month since second dose


R0 <- seq(1,5, by = 0.1)

P = 1 - (1/R0)

plot(R0,P)

beta <- 0.05
gamma <- 0.02
R0 <- beta/gamma

# Parameters and initial conditions.
parameters <- c(beta = beta, gamma = gamma)
initials <- c(S = 1 - 1e-03, I = 1e-03, R = 1 - (1 - 1e-03 - 1e-03))

# Solve and plot.
sir <- SIR(pars = parameters, init = initials, time = 0:500)
PlotMods(sir)

numdays <- 500
days <- seq(0,numdays, by = 1)

N <- 5e6 #population total

event_allowed <- c(0,1)

model_store <- expand_grid(days, dS = 0, S, I, R, R_eff = R0, random_event = 0, event_allowed)


for (event in event_allowed) {
  
  I <- 1e-3*N
  S <- (1 - I/N)*N
  R <- 0
  
  for (day in days) {
    
    random_event_happens <- rbinom(n = 1, size = 1, prob = (1/2)) #An event happens roughly once every two days
    random_event_size <- (I/N)*rpois(n = 1, lambda = 1e4) #The event has 10,000 people
    random_event <- event*random_event_happens*random_event_size
    
    dS = (1/N)*beta*S*I
    
    I <- I + dS - gamma*I + random_event
    S <- S - dS - random_event
    R <- R + gamma*I
    R_eff <- (beta/gamma)*S/N
    
    model_temp <- tibble(days = day, dS, S, I , R, R_eff, random_event, event_allowed = event)
    
    model_store <- 
      rows_update(model_store, model_temp, by = c("days", "event_allowed"))
    
  }
}

model_out <-
  model_store %>% 
  pivot_longer(cols = dS:random_event, names_to = "compartment", values_to = "proportion") %>% 
  mutate(event_allowed = factor(event_allowed, levels = c("0", "1")))

model_plot <-
  model_out %>% 
  ggplot() +
  geom_line(aes(x = days, y = proportion, colour = event_allowed)) +
  facet_grid(rows = vars(factor(compartment, levels = c("dS", "S", "I", "R", "R_eff", "random_event"))), scales = "free_y")

model_plot

model_summary <-
  model_out %>% 
  filter(compartment == "dS") %>% 
  group_by(event_allowed) %>% 
  summarise(sum(proportion))



