library(dplyr)
library(tidyr)
library(googlesheets4)
library(waldo)
params <- readRDS("params.rds")
sp <- species_params(params)

# We check that the species parameters in our MizerParams object agree with
# the parameters in our Google Drive spreadsheet "species_params"
sp_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1mT3JkzooBmkHHNL0B_z9Bhwa__kyt0nnP8xiENVAPRY/edit#gid=201538411")

# Reorder species and species 15: Mediterranean scaldfish
sp_sheet <- sp_sheet[c(1:13, 16:27, 14), ]
compare(sp$species, sp_sheet$species)
compare(sp$latin.name, sp_sheet$`latin name`)

# Check internal consistency of Google sheet

l <- function(w) {
    (w / sp_sheet$a) ^ (1 / sp_sheet$b)
}

w <- function(l) {
    sp_sheet$a * l ^ sp_sheet$b
}

plot_diff <- function(x1, x2) {
    wc <- compare(x1, x2, max_diffs = Inf)
    if (length(wc)) {
        barplot((x1 - x2) / x2, names.arg = 1:length(x1), horiz = TRUE)
    }
    wc
}

plot_diff(sp_sheet$w_inf, w(sp_sheet$adult_max_length))
plot_diff(sp_sheet$w_mat, w(sp_sheet$l_mat))
plot_diff(sp_sheet$w_min, w(sp_sheet$l_min))
plot_diff(sp_sheet$cutoff_size, w(sp_sheet$cutoff_length))

plot_diff(sp$a, sp_sheet$a)
plot_diff(sp$b, sp_sheet$b)

plot_diff(sp$w_inf, sp_sheet$w_inf)

plot_diff(sp$w_mat, sp_sheet$w_mat)

plot_diff(sp$w_min, sp_sheet$w_min)

plot_diff(sp$beta, sp_sheet$beta)
plot_diff(sp$sigma, sp_sheet$sigma)

plot_diff(sp$k_vb, sp_sheet$k_vb)
plot_diff(sp$t0, sp_sheet$t0)

plot_diff(sp$alpha, sp_sheet$alpha)

compare(sp$rho_carrion > 0, sp_sheet$scavQ > 0)
compare(sp$rho_detritus > 0, sp_sheet$detQ > 0)

gp <- gear_params(params)
plot_diff(gp$l50, sp_sheet$l50)
plot_diff(gp$l25, sp_sheet$l25)

plot_diff(sp$biomass_observed, sp_sheet$biomass_observed)
plot_diff(sp$abundance_observed, sp_sheet$abundance_observed)
plot_diff(sp$cutoff_size, sp_sheet$cutoff_size)
