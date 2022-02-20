source("init.R")
catch <- readRDS("catch.rds")
params <- readRDS("params081121.rds")

# Making changes suggested by Francesc
species_params(params)$k_vb[1] <- 1.4
species_params(params)$t0[15] <- -0.198
species_params(params)$w_mat[4] <- species_params(params)$w_mat[4] / 
    2 ^ species_params(params)$b[4]

# Only detritivores should be allowed to eat detritus
species_params(params)$rho_detritus[species_params(params)$detQ == 0] <- 0
params <- setRho(params)

params <- tuneParams(params, 
                tabs = c("Spectra", "Abundance", "Growth", "Repro",
                         "Catch", "Diet", "Death", "Rates", "Sim"),
                controls = c("abundance", "predation", "rho", "fishing", 
                             "reproduction", "other", "interaction"),
                catch = catch)
