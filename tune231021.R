# The abundances in the species_params spreadsheet on 23/10/21 did not agree
# with the abundances in the MizerParams object. I adjusted them, which also
# required an adjustment to `gamma` and `rho`.

library(googlesheets4)
library(mizerExperimental)
source("components.R")
source("helpers.R")
source("plots.R")
source("rhoControl.R")

sp_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1mT3JkzooBmkHHNL0B_z9Bhwa__kyt0nnP8xiENVAPRY/edit#gid=201538411")

params <- readRDS("params.rds")
catch <- readRDS("catch.rds")
params <- setBevertonHolt(params, reproduction_level = 0.25)

# Reorder species and species 15: Mediterranean scaldfish
sp_sheet <- sp_sheet[c(1:4, 7, 5:6, 8:13, 16:24, 27, 25:26, 14), ]
identical(params@species_params$species, sp_sheet$species)

params@species_params$biomass_observed <- sp_sheet$biomass_observed
params@species_params$biomass_cutoff <- sp_sheet$cutoff_size
params@species_params$number_observed <- sp_sheet$abundance_observed
params@species_params$number_cutoff <- sp_sheet$cutoff_size
params@species_params$yield_observed <- sp_sheet$`catch(kg/km2)` / 1000

params@species_params$n_detritus <- params@resource_params$n

params <- tuneParams(params, 
                     tabs = c("Spectra", "Abundance", "Growth", "Repro",
                              "Catch", "Diet", "Death", "Rates", "Sim"),
                     controls = c("abundance", "predation", "rho", "fishing", 
                                  "reproduction", "other", "interaction"),
                     catch = catch)

