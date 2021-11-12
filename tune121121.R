source("init.R")
params <- readRDS("params081121.rds")
catch <- readRDS("catch.rds")

# Implement changes suggested by Francesc
species_params(params)$k_vb[1] <- 1.4
species_params(params)$t0[15] <- -0.198
species_params(params)$w_mat[4] <- species_params(params)$w_mat[4] / 
    2 ^ species_params(params)$b[4]

# Reduce maximum size of resource spectrum
resource_params(params)$w_pp_cutoff <- 1

# Use Silvia's new interaction matrix
theta <- t(as.matrix(read.csv("theta.csv", header = FALSE)))
params <- setInteraction(params, interaction = theta)


params <- tuneParams(params, 
                     tabs = c("Spectra", "Abundance", "Growth", "Repro",
                              "Catch", "Diet", "Death", "Rates", "Sim"),
                     controls = c("abundance", "predation", "rho", "fishing", 
                                  "reproduction", "other", "interaction"),
                     catch = catch)

