source("init.R")

n <- params@initial_n
n_pp <- params@initial_n_pp
n_other <- params@initial_n_other
rates <- getRates(params)

params@other_params$carrion$decompose <- 0
cin <- carrion_biomass_inflow(params, n, rates) / n_other$carrion
cout <- carrion_loss(params, n, rates)
params@other_params$carrion$decompose <- cin - cout

params@other_params$detritus$external <- 0
inflow <- detritus_biomass_inflow(params, n = n, n_other = n_other, rates = rates)
outflow <- detritus_biomass_loss(params, n_pp = n_pp, rates = rates)
params@other_params$detritus$external <- outflow - inflow

sim <- project(params)
plotBiomass(sim)

saveRDS(params, file = "params.rds")
