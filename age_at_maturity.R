source("init.R")
params <- readRDS("params081121.rds")
sp <- params@species_params
a_mat <- - log(1 - (sp$w_mat / sp$w_inf) ^ (1/sp$b)) / sp$k_vb + sp$t0
names(a_mat) <- sp$species
a_mat
