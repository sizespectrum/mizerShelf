#' Total mortality rate in the shelf ecosystem model
#' 
#' @params params A MizerParams object
#' @param n A matrix of species abundances (species x size).
#' @param n_pp A vector of the resource abundance by size
#' @param n_other A list of abundances for other dynamical components of the
#'   ecosystem
#' @param t The time for which to do the calculation (Not used by standard
#'   mizer rate functions but useful for extensions with time-dependent
#'   parameters.)
#' @param f_mort A two dimensional array (species x size) with the fishing
#'   mortality
#' @param pred_mort A two dimensional array (species x size) with the predation
#'   mortality
#' @param ... Unused
#' 
#' @return A named two dimensional array (species x size) with the total
#'   mortality rates.
seMort <- function(params, n, n_pp, n_other, t, f_mort, pred_mort, ...) {
    mizerMort(params, n, n_pp, n_other, t, f_mort, pred_mort, ...) +
        gearMort(params, f_mort = f_mort)
}

#' Excess gear mortality rate
#' 
#' The excess gear mortality rate is the difference between the total
#' gear mortality (as given by the species
#' parameter `gear_mort`) and the fishing mortality) if this is positive.
#' If the total gear mortality does not exceed the fishing mortality then the
#' excess gear mortality is zero.
#' 
#' @param params A MizerParams object
#' @param f_mort The array of fishing mortality rates as calculated by
#'   `getMort()`.
#' @return A named two dimensional array (species x size) with the excess gear
#'   mortality rates.
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
