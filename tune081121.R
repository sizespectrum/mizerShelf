remotes::install_github("sizespectrum/mizerExperimental")
library(dplyr)
library(waldo)
library(mizerExperimental)
source("components.R")
source("helpers.R")
source("plots.R")
source("rhoControl.R")
source("rates.R")

catch <- readRDS("catch.rds")
params <- readRDS("params251021.rds")
p <- params

# Francesc has updated some species parameters
sp <- read.csv("sp_params041121.csv")
compare(species_params(params)$species, sp$species)

sp_old <- read.csv("sp_params231021.csv")
compare(sp, sp_old)

plot_diff(sp$w_inf, sp_old$w_inf)

# Transfer information that is in green or yellow in the excel file ----
species_params(p)$w_inf <- sp$w_inf
for (i in 1:nrow(sp)) {
    p@initial_n[i, p@w > sp$w_inf[[i]]] <- 0
}

species_params(p)$w_mat <- sp$w_mat
species_params(p)$w_mat25 <- sp$w_mat25

gear_params(p)$catchability <- sp$catchability
gear_params(p)$knife_edge_size <- sp$knife_edge_size
gear_params(p)$sel_func <- "knife_edge"
p@gear_params$l25 <- NULL
p@gear_params$l50 <- NULL

species_params(p)$k_vb <- sp$k_vb
species_params(p)$t0 <- sp$t0

species_params(p)$a <- sp$a
species_params(p)$b <- sp$b

# Using abundance data supplied by Francesc
abundances <- read.csv("abundances041121.csv")
abundances$species %in% sp$species
sel <- match(abundances$species, sp$species)
species_params(p)$biomass_observed[sel] <- abundances$biomass / 1000
species_params(p)$biomass_cutoff[sel] <- sp$knife_edge_size[sel]
species_params(p)$yield_observed[sel] <- abundances$catch / 1000

# Make the resource start at the smallest size but keep the old power law
comment(p@cc_pp) <- NULL
comment(p@rr_pp) <- NULL
c <- p@cc_pp[197]
resource_params(p) <- resource_params(p)
p@cc_pp <- p@cc_pp * c / p@cc_pp[197]
p@initial_n_pp <- p@cc_pp

p <- tuneParams(p, 
                     tabs = c("Spectra", "Abundance", "Growth", "Repro",
                              "Catch", "Diet", "Death", "Rates", "Sim"),
                     controls = c("abundance", "predation", "rho", "fishing", 
                                  "reproduction", "other", "interaction"),
                     catch = catch)

# Only detritivores should be allowed to eat detritus
p@other_params$detritus$rho <- p@other_params$detritus$rho * p@species_params$detQ

# Reduce abundances of worms to get closer to the power law community spectrum
p@species_params$number_observed[c(1,3,6,7)] <- 
    p@species_params$number_observed[c(1,3,6,7)] / 50
