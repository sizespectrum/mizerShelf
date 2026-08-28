#' Drive a shelf model to steady state
#'
#' Extends [mizer::tuneSteadyState()] for `mizerShelf` objects: after the
#' consumer abundances have converged, [tune_carrion_detritus()] is called so
#' that the carrion and detritus components are at steady state too.
#'
#' Mizer's own steady-state machinery covers the consumers and the resource and
#' holds any component registered with [mizer::setComponent()] at its stored
#' value, which is why it warns about the carrion component. This method takes
#' responsibility for that component, so the warning is suppressed here.
#'
#' @param params A `mizerShelf` params object.
#' @inheritParams mizer::tuneSteadyState
#' @param ... Passed to [mizer::tuneSteadyState()], for example `t_max`,
#'   `dt`, `distance_tol` or `method`.
#' @return An updated `mizerShelf` object.
#' @seealso [tune_carrion_detritus()]
#' @method tuneSteadyState mizerShelf
#' @export
#' @name tuneSteadyState
tuneSteadyState.mizerShelf <- function(params, solver = c("project", "newton"),
                                       effort = params@initial_effort,
                                       preserve = c("reproduction_level",
                                                    "erepro", "R_max"),
                                       info_level = default_info_level(),
                                       ...) {
    result <- with_info_level(NextMethod(), info_level = info_level,
                              except = "other_components")
    tune_carrion_detritus(result)
}

#' Drive a shelf model to steady state (superseded)
#'
#' `r lifecycle::badge("superseded")` Use [tuneSteadyState()] instead, which is
#' the mizerShelf method that does the same job under the name mizer now
#' prefers. This method is kept so that scripts written against the older
#' [mizer::steady()] keep working unchanged.
#'
#' @param params A `mizerShelf` params object.
#' @inheritParams mizer::steady
#' @return An updated `mizerShelf` object (or a `mizerShelfSim` when
#'   `return_sim = TRUE`).
#' @method steady mizerShelf
#' @export
#' @name steady
steady.mizerShelf <- function(params, t_max = 100, t_per = 1.5, dt = 0.1,
                              t_save = dt, tol = 0.1 * dt,
                              amplitude_tol = 0.01, amp_rel_tol = 0.01,
                              extinction_threshold = 1e-6, return_sim = FALSE,
                              preserve = c("reproduction_level", "erepro",
                                           "R_max"),
                              progress_bar = TRUE,
                              info_level = default_info_level(),
                              method = c("euler", "predictor_corrector",
                                         "tr_bdf2")) {
    result <- NextMethod()
    if (return_sim) {
        sim <- result
        sim@params <- tune_carrion_detritus(sim@params)
        return(sim)
    } else {
        tune_carrion_detritus(result)
    }
}
