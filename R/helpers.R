#' Remove some species from a shelf model
#'
#' Extends [mizer::removeSpecies()] for `mizerShelf` objects: after removing
#' the species it also removes the corresponding rows from the carrion
#' encounter rate matrix `rho`.
#'
#' @param params A `mizerShelf` params object.
#' @param species The species to be removed. A vector of species names, or a
#'   numeric vector of species indices, or a logical vector indicating for each
#'   species whether it is to be removed (TRUE) or not.
#' @param ... Passed to [mizer::removeSpecies()].
#' @return A `mizerShelf` params object with fewer species.
#' @method removeSpecies mizerShelf
#' @examples
#' params <- NWMed_params
#' species_params(params)$species
#' params <- removeSpecies(params, c("Poor cod", "Horse mackerel"))
#' species_params(params)$species
#' @export
removeSpecies.mizerShelf <- function(params, species, ...) {
    keep <- !valid_species_arg(params, species, return.logical = TRUE)
    p <- new("mizerShelf", NextMethod())
    p@other_params$carrion$rho <-
        p@other_params$carrion$rho[keep, , drop = FALSE]
    p
}

#' Add new species
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Takes a \linkS4class{MizerParams} object and adds additional species with
#'   given parameters to the ecosystem. It sets the initial values for these new
#'   species to their steady-state solution in the given initial state of the
#'   existing ecosystem. This will be close to the true steady state if the
#'   abundances of the new species are sufficiently low. Hence the abundances of
#'   the new species are set so that they are at most 1/100th of the resource 
#'   power law. Their reproductive efficiencies are set so as to keep them at
#'   that low level.
#'
#' @param params A mizer params object for the original system.
#' @param species_params Data frame with the species parameters of the new
#'   species we want to add to the system.
#' @param interaction Interaction matrix. A square matrix giving either the
#'   interaction coefficients between all species or only those between the new
#'   species. In the latter case all interaction between an old and a new
#'   species are set to 1. If this argument is missing, all interactions
#'   involving a new species are set to 1.
#' @param gear_params Data frame with the gear parameters for the new
#'   species. If not provided then the new species will not be fished.
#' @param initial_effort A named vector with the effort for new fishing gear
#'   introduced in `gear_params`. New gear for which no effort is set via this
#'   vector will have an initial effort of 0. Should not include effort values
#'   for existing gear.
#'
#' @return An object of type \linkS4class{MizerParams}
#'
#' @details The resulting MizerParams object will use the same size grid where
#'   possible, but if one of the new species needs a larger range of w (either
#'   because a new species has an egg size smaller than those of existing
#'   species or a maximum size larger than those of existing species) then the
#'   grid will be expanded and all arrays will be enlarged accordingly.
#'
#'   If any of the rate arrays of the existing species had been set by the user
#'   to values other than those calculated as default from the species
#'   parameters, then these will be preserved. Only the rates for the new
#'   species will be calculated from their species parameters.
#'
#'   After adding the new species, the background species are not retuned and
#'   the system is not run to steady state. This could be done with [steady()].
#'   The new species will have a reproduction level of 1/4, this can then be
#'   changed with [setBevertonHolt()]
#'
#' @seealso [removeSpecies()]
#' @examples
#' params <- newTraitParams()
#' species_params <- data.frame(
#'     species = "Mullet",
#'     w_max = 173,
#'     w_mat = 15,
#'     beta = 283,
#'     sigma = 1.8,
#'     h = 30,
#'     a = 0.0085,
#'     b = 3.11
#' )
#' params <- addSpecies(params, species_params)
#' plotSpectra(params)
#' @rdname addSpecies
#' @method addSpecies mizerShelf
#' @export
addSpecies.mizerShelf <- function(params, species_params, ..., steady = FALSE) {
    no_old_sp <- nrow(params@species_params)
    no_new_sp <- nrow(species_params)
    n_exp <- params@resource_params[["n"]]
    old_rho <- params@other_params$carrion$rho

    # Let the base method handle all standard bookkeeping.
    # We pass steady = FALSE so it skips steadySingleSpecies, which would fail
    # because rho still has the old species dimensions at that point.
    p <- new("mizerShelf", NextMethod())

    new_sp <- (no_old_sp + 1):(no_old_sp + no_new_sp)
    no_w <- length(p@w)

    # Extend old rho if the size grid was expanded
    no_w_old <- ncol(old_rho)
    if (no_w > no_w_old) {
        old_rho <- cbind(matrix(0, nrow = no_old_sp, ncol = no_w - no_w_old),
                         old_rho)
    }

    # Compute rho_carrion for new species: encounter from detritus alone
    # (temporarily zero-out rho for new species to exclude carrion encounter)
    p@other_params$carrion$rho <-
        rbind(old_rho, matrix(0, nrow = no_new_sp, ncol = no_w))
    E <- getEncounter(p)[new_sp, no_w, drop = FALSE] / p@w[no_w] ^ n_exp

    f0 <- set_species_param_default(p@species_params, "f0", 0.6)$f0[new_sp]
    ic <- set_species_param_default(p@species_params, "interaction_carrion",
                                    1)$interaction_carrion[new_sp]
    rho_carrion_new <- pmax(0, f0 * p@species_params$h[new_sp] / (1 - f0) - E) * ic
    p@species_params$rho_carrion[new_sp] <- rho_carrion_new

    # Rebuild full rho with proper values for new species
    p@other_params$carrion$rho <-
        rbind(old_rho, outer(rho_carrion_new, p@w ^ n_exp))

    # Run steady state and set low abundance for new species
    p@interaction[new_sp, new_sp] <- 0
    p <- steadySingleSpecies(p, species = new_sp)
    for (i in new_sp) {
        idx <- which.max(p@initial_n[i, ] * p@w ^ p@resource_params$lambda)
        p@initial_n[i, ] <- p@initial_n[i, ] *
            p@resource_params$kappa * p@w[idx] ^ (-p@resource_params$lambda) /
            p@initial_n[i, idx] / 100
        p@species_params$is_background[i] <- FALSE
    }
    p@interaction[new_sp, new_sp] <- p@interaction[new_sp, new_sp]

    repro_level <- rep(1 / 4, length(new_sp))
    names(repro_level) <- p@species_params$species[new_sp]
    p <- setBevertonHolt(p, reproduction_level = repro_level)

    p
}