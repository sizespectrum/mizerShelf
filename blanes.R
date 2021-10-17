
remotes::install_github("sizespectrum/mizerExperimental")

library(mizerExperimental)
source("components.R")
source("rhoControl.R")
source("helpers.R")
source("plots.R")

params <- readRDS("params.rds")
catch <- readRDS("catch.rds")

# Study steady state ----
controls <- list("abundance", "predation", "rho", "fishing", "reproduction", "other",
                 "interaction")
params <- tuneParams(params, catch = catch, controls = controls)

# Set component dynamics ----

n <- initialN(params)
rates <- getRates(params)

params@other_params[["carrion"]]$discard <- 0.15
params@other_params[["carrion"]]$external <- 
    params@initial_n_other[["carrion"]] * 
    getLoss(params, n = n, rates = rates, component = "carrion") -
    getInflow(params, n = n, rates = rates, component = "carrion")
params@other_dynamics[["carrion"]] <- "component_dynamics"

params@other_params[["detritus"]]$external <- 
    params@initial_n_other[["detritus"]] * 
    getLoss(params, n = n, rates = rates, component = "detritus") -
    getInflow(params, n = n, rates = rates, component = "detritus")
params@other_dynamics[["detritus"]] <- "component_dynamics"

sim <- project(params, t_max = 10)
plotlyBiomass(sim)

# Sensitivity to fishing ----
params <- setBevertonHolt(params, reproduction_level = 0.5)
sim <- project(params, t_max = 100, effort = 1.1)
plotlyBiomass(sim)

