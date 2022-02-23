# If you have built the mizerNWMed package you can use
# library(mizerNWMed)
# otherwise use
source("inst/init.R")

# set some parameters that control the sensitivity of the system
params <- setBevertonHolt(params, reproduction_level = 0.25)
carrion_lifetime(params) <- 1/365
detritus_lifetime(params) <- 1
carrion_human_origin(params) <- 0.2

params_original <- params

# Change selectivity ----
params <- params_original
gear_params(params)["Hake, Main", c("l25", "l50")]
gear_params(params)["Hake, Main", c("l25", "l50")] <- c(19, 20)

sim <- project(params, t_max = 50)
plotlyBiomass(sim)
plotlyYield(sim)

# Change discards ----
params <- params_original
species_params(params)["Hake", "discard"]
species_params(params)[, "discard"] <- 0

sim <- project(params, t_max = 50)
plotlyBiomass(sim)
plotlyYield(sim)

# Change effort
params <- params_original
sim <- project(params, t_max = 50, effort = 0.9)
plotlyBiomass(sim)
plotlyYield(sim)

# Sensitivity to fishing ----
params <- params_original
F_range <- c(seq(0, 0.5, 0.05), seq(0.6, 1, 0.1), seq(1, 2, 1))
#params <- setBevertonHolt(params, reproduction_level = 0.1)
plotYieldVsF(params, species = "Red mullet", F_range = F_range)
