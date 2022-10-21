# We check that the species parameters in our MizerParams object agree with
# the parameters in our Google Drive spreadsheet "params231121_streamlined2"

library(dplyr)
library(tidyr)
library(googlesheets4)
library(waldo)
library(mizerExperimental)
library(mizerNWMed)

params <- readParams("inst/params171022.rds")
sp <- species_params(params)
sp_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1mXJ4BMeO6V95-B58UqQ-ZxqHcQuTXQnN5_Wa17zC7CE/edit#gid=931771299",
                       na = "NA")

# The species have been renamed as requested by Silvia
compare(sp$species, sp_sheet$species, max_diffs = 30)

compare(sp$w_mat, sp_sheet$w_mat, max_diffs = 30)
compare(sp$w_mat25, sp_sheet$w_mat25, max_diffs = 30)
compare(sp$w_inf, sp_sheet$w_inf, max_diffs = 30)
compare(sp$w_min, sp_sheet$w_min, max_diffs = 30)

compare(sp$k_vb, sp_sheet$k_vb, max_diffs = 30)
compare(sp$t0, sp_sheet$t0, max_diffs = 30)
compare(sp$a, sp_sheet$a, max_diffs = 30)
compare(sp$b, sp_sheet$b, max_diffs = 30)

compare(sp$biomass_observed, sp_sheet$biomass_observed, max_diffs = 30)
compare(sp$biomass_cutoff, sp_sheet$biomass_cutoff, max_diffs = 30)
compare(sp$number_observed, sp_sheet$number_observed, max_diffs = 30)
compare(sp$number_cutoff, sp_sheet$number_cutoff, max_diffs = 30)



gp <- gear_params(params)
compare(gp$l50, sp_sheet$l50, max_diffs = 30)
compare(gp$l25, sp_sheet$l25, max_diffs = 30)

compare(sp$biomass_observed, sp_sheet$biomass_observed, max_diffs = 30)
compare(sp$number_observed, sp_sheet$number_observed, max_diffs = 30)
compare(sp$biomass_cutoff, sp_sheet$biomass_cutoff, max_diffs = 30)
compare(sp$number_cutoff, sp_sheet$number_cutoff, max_diffs = 30)
