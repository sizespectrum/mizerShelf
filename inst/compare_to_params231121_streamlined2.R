# We check that the species parameters in our MizerParams object agree with
# the parameters in our Google Drive spreadsheet "params231121_streamlined2"

library(dplyr)
library(tidyr)
library(googlesheets4)
library(waldo)
library(mizerExperimental)
library(mizerShelf)
params <- NWMed_params

sp <- species_params(params)
sp_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1mXJ4BMeO6V95-B58UqQ-ZxqHcQuTXQnN5_Wa17zC7CE/edit#gid=931771299",
                       na = "NA")

# The species have been renamed as requested by Silvia
compare(sp$species, sp_sheet$species, max_diffs = 30)

# Check internal consistency of Google sheet
l_from_w <- function(w) {
    (w / sp_sheet$a) ^ (1 / sp_sheet$b)
}
w_from_l <- function(l) {
    sp_sheet$a * l ^ sp_sheet$b
}
compare(sp_sheet$w_inf, w_from_l(sp_sheet$adult_max_length), max_diffs = 30,
        tolerance = 0.001)
compare(sp_sheet$w_mat, w_from_l(sp_sheet$l_mat), max_diffs = 30,
        tolerance = 0.001)

compare(sp$w_mat, signif(sp_sheet$w_mat, 3), max_diffs = 30)
compare(sp$w_mat25, signif(sp_sheet$w_mat25, 3), max_diffs = 30)
compare(sp$w_inf, signif(sp_sheet$w_inf, 3), max_diffs = 30)
compare(sp$w_min, signif(sp_sheet$w_min, 3), max_diffs = 30)

compare(sp$k_vb, signif(sp_sheet$k_vb, 3), max_diffs = 30)
compare(sp$t0, sp_sheet$t0, max_diffs = 30)
compare(sp$a, sp_sheet$a, max_diffs = 30)
compare(sp$b, sp_sheet$b, max_diffs = 30)

# We don't use the biomass for species where numbers have also been observed
compare(sp$biomass_observed, sp_sheet$biomass_observed, max_diffs = 30)
compare(sp$biomass_cutoff, sp_sheet$biomass_cutoff, max_diffs = 30)

compare(sp$number_observed, sp_sheet$number_observed, max_diffs = 30)
compare(sp$number_cutoff, sp_sheet$number_cutoff, max_diffs = 30)

compare(sp$yield_observed, sp_sheet$yield_observed, max_diffs = 30)

gp <- gear_params(params)
compare(gp$l50, signif(sp_sheet$l50, 3), max_diffs = 30)
compare(gp$l25, signif(sp_sheet$l25, 3), max_diffs = 30)

