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

#' @rdname mizerShelf-class
#' @export
setClass("mizerShelf", contains = "MizerParams")

#' @rdname mizerShelf-class
#' @export
setClass("mizerShelfSim", contains = "MizerSim")

# getBiomass --------------------------------------------------------------

#' Get biomass of species and components for a shelf model
#'
#' Extends [mizer::getBiomass()] for `mizerShelf` and `mizerShelfSim` objects
#' by adding the detritus (resource spectrum) and any scalar other-component
#' biomasses (e.g. carrion).
#'
#' @param object A `mizerShelf` or `mizerShelfSim` object.
#' @param ... Passed to the base [mizer::getBiomass()] method.
#' @return For `mizerShelf`: a named numeric vector of species/component
#'   biomasses. For `mizerShelfSim`: an `ArrayTimeBySpecies` matrix
#'   (time x species/component) with species biomasses followed by Detritus
#'   and other component biomasses.
#' @examples
#' getBiomass(NWMed_params)
#' \donttest{
#' sim <- project(NWMed_params, t_max = 3)
#' getBiomass(sim)
#' }
#' @method getBiomass mizerShelf
#' @export
#' @name getBiomass
getBiomass.mizerShelf <- function(object, ...) {
    params <- object
    b <- NextMethod()

    d_biomass <- sum(params@initial_n_pp * params@dw_full * params@w_full)
    b <- c(b, Detritus = d_biomass)

    other <- params@initial_n_other
    scalar_other <- Filter(function(x) is.numeric(x) && length(x) == 1, other)
    if (length(scalar_other) > 0) {
        b <- c(b, unlist(scalar_other))
    }
    b
}

#' @method getBiomass mizerShelfSim
#' @export
getBiomass.mizerShelfSim <- function(object, ...) {
    sim <- object
    params <- sim@params
    b <- unclass(NextMethod())
    dimname_names <- names(dimnames(b))

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
    names(dimnames(b)) <- dimname_names

    ArrayTimeBySpecies(b, value_name = "Biomass", units = "g",
                       params = params)
}
