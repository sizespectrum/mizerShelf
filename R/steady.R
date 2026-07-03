#' Drive a shelf model to steady state
#'
#' Extends [mizer::steady()] for `mizerShelf` objects: after the fish
#' abundances have converged, [tune_carrion_detritus()] is called so that the
#' carrion and detritus components are also at steady state.
#'
#' @param params A `mizerShelf` params object.
#' @inheritParams mizer::steady
#' @param ... Passed to [mizer::steady()], for example `preserve`,
#'   `progress_bar`, `info_level` or `method`.
#' @return An updated `mizerShelf` object (or a `mizerShelfSim` when
#'   `return_sim = TRUE`).
#' @method steady mizerShelf
#' @export
#' @name steady
steady.mizerShelf <- function(params, t_max = 100, t_per = 1.5, dt = 0.1,
                              tol = 0.1 * dt, return_sim = FALSE, ...) {
    result <- NextMethod()
    if (return_sim) {
        sim <- result
        sim@params <- tune_carrion_detritus(sim@params)
        return(sim)
    } else {
        tune_carrion_detritus(result)
    }
}
