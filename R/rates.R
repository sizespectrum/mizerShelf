seMort <- function(params, n, n_pp, n_other, t, f_mort, pred_mort, ...) {
    mizerMort(params, n, n_pp, n_other, t, f_mort, pred_mort, ...) +
        gearMort(params, f_mort = f_mort)
}

gearMort <- function(params, f_mort) {
    gear_mort <- params@species_params$gear_mort - f_mort
    gear_mort[gear_mort < 0] <- 0
    gear_mort
}

# predation kernel that cuts off at 1 sigma
cut_lognormal_pred_kernel <- function(ppmr, beta, sigma) {
    Beta <- log(beta)
    phi <- exp(-(log(ppmr) - Beta)^2 / (2 * sigma^2))
    # rr is the maximal log predator/prey mass ratio
    rr <- exp(Beta + sigma)
    phi[ppmr > rr] <- 0
    return(phi)
}
