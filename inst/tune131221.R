# Turn on carrion dynamics

source("init.R")
params <- readRDS("params101221.rds")

rates <- getRates(params)
n <- params@initial_n
params@other_params$carrion$decompose <- 0
getLoss(params, n, rates) * params@initial_n_other$carrion
getInflow(params, n, rates)

params@other_params$carrion$decompose <-
    getInflow(params, n, rates) / params@initial_n_other$carrion -
    getLoss(params, n, rates)

getInflow(params, n, rates) - 
    getLoss(params, n, rates) * params@initial_n_other$carrion

params@other_dynamics$carrion <- "carrion_dynamics"

sim <- project(params)
