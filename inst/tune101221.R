source("init.R")

catch <- readRDS("catch.rds")
params <- readRDS("params091221.rds")
sp <- params@species_params

# Make adjustments to external mortality
sp$z0[sp$z0 < 0.3] <- 0.1
sp$z0[sp$z0 >= 0.3] <- 1
sp$z0[1] <- 10
sp$z0[3] <- 5
sp$z0[6] <- 8
sp$z0[7] <- 10
species_params(params)$z0 <- sp$z0

controls <- list("abundance", "predation", "rho", "fishing", "reproduction", "other",
                 "interaction")
params <- tuneParams(params, catch = catch, controls = controls)

saveRDS(params, file = "params101221.rds")
