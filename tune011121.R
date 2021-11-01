# Silvia and Francesc have made some parameter changes. I will read them
# from their excel file into the latest params object

# Load code and data ----
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
sp <- read.csv("sp_params231021.csv")
compare(species_params(params)$species, sp$species)

# Check consistency between lengths and weigths ----
l <- function(w) {
    (w / sp$a) ^ (1 / sp$b)
}

w <- function(l) {
    sp$a * l ^ sp$b
}

plot_diff <- function(x1, x2) {
    wc <- compare(x1, x2, max_diffs = Inf)
    if (length(wc)) {
        barplot((x1 - x2) / x2, names.arg = 1:length(x1), horiz = TRUE)
    }
    wc
}

plot_diff(sp$w_inf, w(sp$adult_max_length))
plot_diff(sp$w_mat, w(sp$l_mat))
plot_diff(sp$w_min, w(sp$l_min))
plot_diff(sp$cutoff_size, w(sp$cutoff_length))

# Transfer information that is in green or yellow in the excel file ----
species_params(p)$w_inf <- sp$w_inf
for (i in 1:nrow(sp)) {
    p@initial_n[i, p@w > sp$w_inf[[i]]] <- 0
}

species_params(p)[16:26, c("w_mat", "w_mat25")] <- sp[16:26, c("w_mat", "w_mat25")]

gear_params(p)$catchability[16:26] <- sp$catchability[16:26]
# Not yet transferring knife-edge selectivity, awaiting response from Silvia

species_params(p)$k_vb <- sp$k_vb
species_params(p)$t0[20] <- sp$t0[20]

species_params(p)$a <- sp$a
species_params(p)$b <- sp$b

# Check that abundance info has not changed ----
compare(p@species_params$number_cutoff, sp$number_cutoff, tolerance = 0.0001)
compare(p@species_params$number_observed, sp$number_observed, tolerance = 0.0001)
compare(p@species_params$yield_observed, sp$yield_observed, tolerance = 0.0001)

# Tune ----
controls <- list("abundance", "predation", "rho", "fishing", "reproduction", "other",
                 "interaction")
p <- tuneParams(p, catch = catch, controls = controls)
