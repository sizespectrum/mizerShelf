#' Remove some species from the model
#' 
#' This calls `mizer::removeSpecies()` and in addition removes the relevant
#' row from the carrion consumption array `rho`.
#' @param params A MizerParams object
#' @param species The species to be removed. A vector of species names, or a
#'   numeric vector of species indices, or a logical vector indicating for each
#'   species whether it is to be removed (TRUE) or not.
#' @return A MizerParams object with fewer species.
#' @examples
#' params <- NWMed_params
#' species_params(params)$species
#' params <- removeSpecies(params, c("Poor cod", "Horse mackerel"))
#' species_params(params)$species
#' @export
removeSpecies <- function(params, species) {
    p <- mizer::removeSpecies(params, species)
    species <- valid_species_arg(params, species,
                                 return.logical = TRUE)
    keep <- !species
    p@other_params$carrion$rho <-
        p@other_params$carrion$rho[keep, , drop = FALSE]
    p
}