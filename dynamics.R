encounter_contribution <- function(params, n_other, component, ...) {
    params@other_params[[component]]$rho * n_other[[component]]
}

constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}

#' Detritus dynamics
#'
#' Calculates the detritus biomass at the next timestep from the current
#' detritus biomass.
#'
#' The equation for the time evolution of the detritus biomass \eqn{B} is
#' assumed to be of the form
#' \deqn{dB/dt = \tt{inflow} - \tt{consumption} * B + \tt{external}}{dB/dt = inflow - consumption * B + external}
#' where
#' * `inflow` comes from feces, calculated as a proportion
#'   `params@other_params$detritus$proportion` of the biomass consumed by all
#'   consumers.
#' * `consumption` is by detritivorous species, where the encounter rate is
#'   specified by `params@other_params$detritus$rho`.
#' * `external` is an influx from external sources. It can be negative in which
#'   case it represents a loss to external sources. It is stored in
#'   `params@other_params$detritus$external`
#'
#' This equation is solved analytically to
#' \deqn{B(t+dt) = B(t)\exp(-\tt{consumption} \cdot dt)
#'   +\frac{\tt{inflow} + \tt{external}}{\tt{consumption}}
#'   (1-\exp(-\tt{consumption} \cdot dt)).}{B(t+dt)
#'   = B(t) exp(-consumption * dt)
#'   +(inflow + external)/(consumption) * (1 - exp(-consumption * dt)).}
#' This avoids the stability problems that would arise if we used the Euler
#' method to solve the equation numerically.
#'
#' @param params A [MizerParams] object
#' @param n A matrix of current species abundances (species x size)
#' @param n_other List of abundances of other dynamic components
#' @param rates A list of rates as returned by [getRates()]
#' @param dt Time step size
#' @param ... Unused
#'
#' @return A single number giving the biomass of detritus at next time step
#' @export
#' @family resource dynamics functions
#' @md
detritus_dynamics <-
    function(params, n, n_other, rates, dt, ...) {

        consumption <- getConsumptionByFish(params, "detritus", n, rates)
        inflow <-
            params@other_params$detritus$proportion *
            sum((rates$feeding_level * params@intake_max * n) %*% params@dw)

        if (consumption) {
            et <- exp(-consumption * dt)
            return(n_other[["detritus"]] * et +
                       (inflow  + params@other_params$detritus$external) /
                       consumption  * (1 - et))
        }
        return(n_other[["detritus"]] +
                   (inflow  + params@other_params$detritus$external) * dt)
    }

#' Carrion dynamics
#'
#' Calculates the biomass of carrion (dead animals) at the next timestep from
#' the current biomass.
#'
#' The equation for the time evolution of the carrion biomass \eqn{B} is
#' assumed to be of the form
#' \deqn{dB/dt = inflow - consumption * B + external}
#' where
#' * `inflow` comes from
#'     + Discards from fishing.
#'     + Animals killed by fishing gear.
#'     + Animals that have died by causes other than predation.
#' * `consumption` is by scavenger species, where the encounter rate is
#'   specified by `params@other_params$carrion$rho`.
#' * `external` is an influx from external sources. It can be negative in which
#'   case it represents a loss to external sources. It is stored in
#'   `params@other_params$carrion$external`
#'
#' This equation is solved analytically to
#' \deqn{B(t+dt) = B(t)\exp(-\tt{consumption} \cdot dt)
#'   +\frac{\tt{inflow} + \tt{external}}{\tt{consumption}}
#'   (1-\exp(-\tt{consumption} \cdot dt)).}{B(t+dt)
#'   = B(t) exp(-consumption * dt)
#'   +(inflow + external)/(consumption) * (1 - exp(-consumption * dt)).}
#' This avoids the stability problems that would arise if we used the Euler
#' method to solve the equation numerically.
#'
#' @inheritParams detritus_dynamics
#' @param ... Unused
#'
#' @return A single number giving the biomass of carrion at next time step
#' @export
#' @family resource dynamics functions
#' @md
carrion_dynamics <-
    function(params, n, n_other, rates, dt, ...) {

        inflow <-
            # still need to be written
            0

        consumption <- getConsumptionByFish(params, "carrion", n, rates)
        if (consumption) {
            et <- exp(-consumption * dt)
            return(n_other[["carrion"]] * et +
                       (inflow  + params@other_params$carrion$external) /
                       consumption  * (1 - et))
        }
        return(n_other[["carrion"]] +
                   (inflow  + params@other_params$carrion$external) * dt)
    }

#' Get rate of consumption of resource by fish
#'
#' This function is used by the dynamics functions, for example
#' `detritus_dynamics()` and `carrion_dynamics()`, in their calculation of the
#' resource biomasses at the next time step. Not exported.
#'
#' @param params The MizerParams object
#' @param resource A string with the name of the resource for which the
#'   consumption is to be calculated
#' @param n A matrix with the current size spectrum of fish
#' @param rates A list with the rates
#'
#' @return A number giving the rate at which the resource is consumed by all
#'   the fish in units of grams/year
#' @md
getConsumptionByFish <- function(params, resource, n, rates) {
    sum((params@other_params[[resource]]$rho * n *
             (1 - rates$feeding_level)) %*% params@dw)
}
