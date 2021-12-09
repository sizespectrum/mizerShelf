encounter_contribution <- function(params, n_other, component, ...) {
    params@other_params[[component]]$rho * n_other[[component]]
}

#' Dynamics of the unstructured components
#'
#' Calculates the component biomass at the next timestep from the current
#' component biomass.
#'
#' The equation for the time evolution of the component biomass \eqn{B} is
#' assumed to be of the form
#' \deqn{dB/dt = \tt{inflow} - \tt{loss} * B + \tt{external}}{dB/dt = inflow - loss * B + external}
#' where
#' * `loss` is the mass-specific rate of loss due to consumption, where the
#'   encounter rate is specified by `params@other_params[[component]]$rho`.
#' * `external` is an influx from external sources. It can be negative in which
#'   case it represents a loss to external sources. It is specified by
#'   `params@other_params[[component]]$external`.
#' * `inflow` is the rate at which the rest of the system produces biomass that
#'   enters this component. Its calculation depends on the type of component:
#'   + For detritus it comes from feces and is calculated as a proportion 
#'   `params@other_params[["detritus"]]$proportion` of the biomass consumption
#'   rate of all consumers.
#'   + For carrion (corpses) the inflow comes from three sources: 1) animals
#'   killed by the fishing gear, 2) animals that have died by natural causes
#'   other than predation, 3) discards from fishing.
#'
#' The dynamical equation is solved analytically to
#' \deqn{B(t+dt) = B(t)\exp(-\tt{loss} \cdot dt)
#'   +\frac{\tt{inflow} + \tt{external}}{\tt{loss}}
#'   (1-\exp(-\tt{loss} \cdot dt)).}{B(t+dt)
#'   = B(t) exp(-loss * dt)
#'   +(inflow + external)/(loss) * (1 - exp(-loss * dt)).}
#' This avoids the stability problems that would arise if we used the Euler
#' method to solve the equation numerically.
#'
#' @param params A [MizerParams] object
#' @param n A matrix of current species abundances (species x size)
#' @param n_pp A vector of current plankton abundance by size
#' @param n_other List of abundances of other dynamic components
#' @param rates A list of rates as returned by [getRates()]
#' @param t Current time
#' @param dt Time step size
#' @param component 
#' @param ... Unused
#'
#' @return A single number giving the component biomass at next time step
#' @export
#' @md
component_dynamics <-
    function(params, n, n_pp, n_other, rates, dt, component, ...) {
        
        loss <- getLoss(params, n, rates, component)
            
        external <- params@other_params[[component]]$external
        
        inflow <- getInflow(params, n, rates, component)
        
        if (loss) {
            et <- exp(-loss * dt)
            return(n_other[[component]] * et +
                       (inflow  + external) / loss  * (1 - et))
        }
        return(n_other[[component]] + (inflow  + external) * dt)
    }

getLoss <- function(params, n, rates, component) {
    sum((params@other_params[[component]]$rho * n *
             (1 - rates$feeding_level)) %*% 
            params@dw)
}

getInflow <- function(params, n, rates, component) {
    if (component == "detritus") {
        inflow <-
            sum(((rates$feeding_level * params@intake_max * n) %*% params@dw) *
                    (1 - params@species_params$alpha))
    } else if (component == "carrion") {
        inflow <-
            sum(((params@mu_b + gearMort(params, rates$f_mort)) * n) %*% 
                     (params@w * params@dw)) +
            sum(((rates$f_mort * n) %*% (params@w * params@dw)) *
                    params@species_params$discard)
    }
    inflow
}

constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}
