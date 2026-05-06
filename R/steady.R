#' Drive a shelf model to steady state
#'
#' Extends [mizer::steady()] for `mizerShelf` objects: after the fish
#' abundances have converged, [tune_carrion_detritus()] is called so that the
#' carrion and detritus components are also at steady state.
#'
#' @param params A `mizerShelf` params object.
#' @inheritParams mizer::steady
#' @return An updated `mizerShelf` object (or a `mizerShelfSim` when
#'   `return_sim = TRUE`).
#' @method steady mizerShelf
#' @export
#' @rdname steady
steady.mizerShelf <- function(params, t_max = 100, t_per = 1.5, dt = 0.1,
                              tol = 0.1 * dt, return_sim = FALSE, ...) {
    result <- NextMethod()
    if (return_sim) {
        sim <- new("mizerShelfSim", result)
        sim@params <- tune_carrion_detritus(new("mizerShelf", sim@params))
        return(sim)
    } else {
        tune_carrion_detritus(new("mizerShelf", result))
    }
}
