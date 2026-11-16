#' mizerShelf extension classes
#'
#' S3 extension classes for [MizerParams] and [MizerSim] that enable S3 dispatch
#' for extension upgrading. Measurements, tuning and structural changes
#' use stored capabilities; they share mizer's ordinary methods.
#'
#' The class names are ordinary entries in the object's S3 class vector. All
#' extension-specific data lives in `other_params(params)` or in the component
#' parameters of the "carrion" component (see [setComponent()]).
#'
#' Objects of class `mizerShelf` are created by [newDetritusCarrionParams()].
#' Objects of class `mizerShelfSim` are returned automatically by [project()]
#' when called on a `mizerShelf` params object.
#'
#' No class declaration is needed. [newDetritusCarrionParams()] records the
#' extension on the object with [mizer::recordExtension()] and then calls
#' [mizer::coerceToExtensionClass()]. For example, the params class vector is
#' `c("mizerShelf", "MizerParams")`; simulations created by [project()] receive
#' `c("mizerShelfSim", "MizerSim")` automatically.
#'
#' @name mizerShelf-class
#' @keywords internal
NULL
