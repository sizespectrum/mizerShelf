#' mizerShelf marker classes
#'
#' S4 marker subclasses of [MizerParams] and [MizerSim] that enable S3 dispatch
#' for shelf-specific methods such as [steady()], [scaleModel()],
#' [removeSpecies()], [addSpecies()], and [getBiomass()].
#'
#' Objects of class `mizerShelf` are created by [newDetritusCarrionParams()].
#' Objects of class `mizerShelfSim` are returned automatically by [project()]
#' when called on a `mizerShelf` params object.
#'
#' @name mizerShelf-class
NULL

#' @export
setClass("mizerShelf", contains = "MizerParams")

#' @export
setClass("mizerShelfSim", contains = "MizerSim")

# validParams -------------------------------------------------------------

#' @method validParams mizerShelf
#' @export
validParams.mizerShelf <- function(params, ...) {
    new("mizerShelf", NextMethod())
}

# project -----------------------------------------------------------------

#' @method project mizerShelf
#' @export
project.mizerShelf <- function(params, ...) {
    new("mizerShelfSim", NextMethod())
}

#' @method project mizerShelfSim
#' @export
project.mizerShelfSim <- function(sim, ...) {
    new("mizerShelfSim", NextMethod())
}

# getBiomass --------------------------------------------------------------

#' Get biomass of species and components through time for a shelf model
#'
#' Extends [mizer::getBiomass()] for `mizerShelfSim` objects by adding the
#' detritus (resource spectrum) and any scalar other-component biomasses (e.g.
#' carrion) as extra columns.
#'
#' @param object A `mizerShelfSim` object.
#' @param ... Passed to the base [mizer::getBiomass()] method.
#' @return An `ArraySpeciesByTime` matrix (time x species/component) with
#'   species biomasses followed by Detritus and other component biomasses.
#' @method getBiomass mizerShelfSim
#' @export
#' @rdname getBiomass
getBiomass.mizerShelfSim <- function(object, ...) {
    sim <- object
    params <- sim@params
    b <- unclass(NextMethod())

    # Add detritus (resource spectrum n_pp)
    d_biomass <- rowSums(sweep(sim@n_pp, 2,
                               params@dw_full * params@w_full, "*"))
    b <- cbind(b, Detritus = d_biomass)

    # Add scalar other dynamical components (e.g. carrion)
    if (length(dim(sim@n_other)) >= 2 && ncol(sim@n_other) > 0) {
        comp_names <- dimnames(sim@n_other)[[2]]
        comp_mat <- matrix(unlist(sim@n_other),
                           nrow = nrow(sim@n_other),
                           ncol = ncol(sim@n_other))
        colnames(comp_mat) <- comp_names
        b <- cbind(b, comp_mat)
    }

    ArraySpeciesByTime(b, value_name = "Biomass", units = "g",
                       params = params)
}
