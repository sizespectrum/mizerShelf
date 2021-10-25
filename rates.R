seMort <- function(params, n, n_pp, n_other, t, f_mort, pred_mort, ...) {
    mizerMort(params, n, n_pp, n_other, t, f_mort, pred_mort, ...) +
        gearMort(params, f_mort = f_mort)
}

gearMort <- function(params, f_mort) {
    gear_mort <- params@species_params$gearMort - f_mort
    gear_mort[gear_mort < 0] <- 0
    gear_mort
}