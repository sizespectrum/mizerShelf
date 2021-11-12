# Here is an attempt to introduce a separate resource spectrum for the
# mesopelagic fish with a benthic phase.
# However it is not clear that these play a sufficiently big role to be worth
# the effort of including them.
install_github("sizespectrum/mizerMR")
library(mizerMR)
source("init.R")
params <- readRDS("params081121.rds")

rp <- as.data.frame(resource_params(params))

mp <- rp
mp$resource <- "Mesopelagic"
mp$w_min <- 1
mp$w_max <- rp$w_pp_cutoff
mp$kappa <- rp$kappa / 10

rp$resource <- "Resource"
rp$w_min <- 1e-9
rp$w_max <- 1

rp <- rbind(rp, mp)
rp$w_pp_cutoff <- NULL

resource_params(params) <- rp
resource_interaction(params)[, 2] <- params@species_params$interaction_resource
plotlySpectra(params, total = TRUE, power = 2)

plotGrowthCurves(params, species_panel = TRUE)