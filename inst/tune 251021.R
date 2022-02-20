remotes::install_github("sizespectrum/mizerExperimental")
library(googlesheets4)
library(mizerExperimental)
source("components.R")
source("helpers.R")
source("plots.R")
source("rhoControl.R")
source("rates.R")

params <- readRDS("params231021.rds")
params <- setRateFunction(params, "Mort", "seMort")

# get gear mortality from species_params spreadsheet
sp_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1mT3JkzooBmkHHNL0B_z9Bhwa__kyt0nnP8xiENVAPRY/edit#gid=201538411")
sp_sheet <- sp_sheet[c(1:4, 7, 5:6, 8:13, 16:24, 27, 25:26, 14), ]
params@species_params$gearMort <- sp_sheet$`Gear mortality`
params@species_params$discard <- sp_sheet$`%discards`


plotlyDeath(params, species = "Hake", proportion = FALSE)

params <- tuneParams(params, 
                     tabs = c("Spectra", "Abundance", "Growth", "Repro",
                              "Catch", "Diet", "Death", "Rates", "Sim"),
                     controls = c("abundance", "predation", "rho", "fishing", 
                                  "reproduction", "other", "interaction"),
                     catch = catch)
