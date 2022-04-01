# If you have built the mizerNWMed package you can use
#library(mizerNWMed)
# otherwise use
source("inst/init.R")

# set some parameters that control the sensitivity of the system
params <- setBevertonHolt(params, reproduction_level = 0.25)
carrion_lifetime(params) <- 1/365
detritus_lifetime(params) <- 1
carrion_human_origin(params) <- 0.2

params_original <- params

# Baseline ----
params <- params_original
gear_params(params)["Hake, Main", c("l25", "l50")]
bas <- project(params, t_max = 50)
plotlyBiomass(bas)
plotlyYield(bas)

source("R/plots.R")

brb <- plotBiomass(bas, return_data = TRUE)
write.csv(brb, "biomass_baseline.csv")
bry <- plotYield(sim, return_data = TRUE)
write.csv(bry, "yield_baseline.csv")

# Change selectivity ----
params <- params_original
gear_params(params)["Red mullet, Main", c("l25", "l50")]
gear_params(params)["Red mullet, Main", c("l25", "l50")] <- c(15.13, 17.28)
gear_params(params)["Hake, Main", c("l25", "l50")]
gear_params(params)["Hake, Main", c("l25", "l50")] <- c(19.75, 21.5)
gear_params(params)["Angler fish, Main", c("l25", "l50")] <- c(14, 18)
gear_params(params)["Poor cod, Main", c("l25", "l50")] <- c(8.1, 9.5)
gear_params(params)["Horse mackerel, Main", c("l25", "l50")] <- c(15.2, 17)
gear_params(params)["Shortfin squid, Main", c("l25", "l50")] <- c(14.2, 16.5)
gear_params(params)["Blue whiting, Main", c("l25", "l50")] <- c(18.80, 22.25)
gear_params(params)["Striped red mullet, Main", c("l25", "l50")] <- c(15.13, 17.28)
gear_params(params)["Horned octopus, Main", c("l25", "l50")] <- c(14.20, 16.15)


sim <- project(params, t_max = 50)
plotlyBiomass(sim)
plotlyYield(sim)

source("R/plots.R")

fr <- plotBiomass(sim, return_data = TRUE)
write.csv(fr, "biomass_sel_target.csv")
gr <- plotYield(sim, return_data = TRUE)
write.csv(gr, "yield_sel_target.csv")


# Change discards ----
params <- params_original
species_params(params)["Hake", "discard"] <- 0
species_params(params)[, "discard"] <- 0

sim <- project(params, t_max = 50)
plotlyBiomass(sim)
plotlyYield(sim)

lr <- plotBiomass(sim, return_data = TRUE)
write.csv(lr, "biomass_dis.csv")
mr <- plotYield(sim, return_data = TRUE)
write.csv(mr, "yield_dis.csv")


# Change effort
params <- params_original
sim <- project(params, t_max = 50, effort = 0.6)
plotlyBiomass(sim)
plotlyYield(sim)

nr <- plotBiomass(sim, return_data = TRUE)
write.csv(nr, "biomass_effort.csv")
mr <- plotYield(sim, return_data = TRUE)
write.csv(mr, "yield_effort.csv")

# Sensitivity to fishing ----
params <- params_original
F_range <- c(seq(0, 0.5, 0.05), seq(0.6, 1, 0.1), seq(1, 2, 1))
#params <- setBevertonHolt(params, reproduction_level = 0.1)
plotYieldVsF(params, species = "Red mullet", F_range = F_range)
