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
#' @param n_other List of abundances of other dynamic components
#' @param rates A list of rates as returned by [getRates()]
#' @param dt Time step size
#' @param ... Unused
#'
#' @return A single number giving the component biomass at next time step
#' @export
#' @md
#' 

carrion_biomass <- function(params) {
    params@initial_n_other$carrion
}

carrion_dynamics <-
    function(params, n, n_other, rates, dt, ...) {
        
        loss <- carrion_loss(params, n, rates)
        inflow <- sum(carrion_biomass_inflow(params, n, rates))
        
        if (loss) {
            et <- exp(-loss * dt)
            return(n_other$carrion * et + inflow / loss  * (1 - et))
        }
        return(n_other$carrion + inflow * dt)
    }

carrion_loss <- function(params, n = params@initial_n, 
                         rates = getRates(params)) {
    sum((params@other_params$carrion$rho * n *
             (1 - rates$feeding_level)) %*% 
            params@dw) +
        params@other_params$carrion$decompose
}

carrion_biomass_inflow <- function(params, n = params@initial_n, 
                                          rates = getRates(params)) {
    c(mu_b = sum((params@mu_b * n) %*% (params@w * params@dw)) *
          params@other_params$carrion$ext_prop,
      gear_mort = sum((gearMort(params, rates$f_mort) * n) %*% 
                          (params@w * params@dw)),
      discards = sum(((rates$f_mort * n) %*% (params@w * params@dw)) *
                         params@species_params$discard)
    )
}

constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}

detritus_biomass <- function(params, n_pp = params@initial_n_pp) {
    sum(n_pp * params@dw_full * params@w_full)
}

detritus_dynamics <- function(params, n, n_pp, n_other, rates, dt, ...) {
    current_biomass <- detritus_biomass(params, n_pp = n_pp)
    loss <- detritus_biomass_loss(params, n_pp, rates) / current_biomass
    inflow <- sum(detritus_biomass_inflow(params, n, n_other, rates))
    
    if (loss) {
        et <- exp(-loss * dt)
        next_biomass <- current_biomass * et + inflow / loss  * (1 - et)
    } else {
        next_biomass <- current_biomass + inflow * dt
    }
    n_pp * next_biomass / current_biomass
}

detritus_biomass_loss <- function(params, n_pp = params@initial_n_pp, 
                                  rates = getRates(params)) {
    sum(rates$resource_mort * n_pp * params@w_full * params@dw_full)
}

detritus_biomass_inflow <- function(params, n = params@initial_n,
                                    n_other = params@initial_n_other,
                                    rates = getRates(params)) {
    consumption <- sweep((1 - rates$feeding_level) * rates$encounter * n, 2,
                         params@dw, "*", check.margin = FALSE)
    feces <- sweep(consumption, 1, (1 - params@species_params$alpha), "*", 
                   check.margin = FALSE)
    carrion <- params@other_params$carrion$decompose * n_other$carrion
    c(feces = sum(feces),
      carrion = carrion,
      external = params@other_params$detritus$external
    )
}

carrion_lifetime <- function(params) {
    1 / carrion_loss(params)
}

`carrion_lifetime<-` <- function(params, value) {
    rescale_carrion(params, value / carrion_lifetime(params))
}

detritus_lifetime <- function(params) {
    current_biomass <- 
        sum(params@initial_n_pp * params@dw_full * params@w_full)
    current_biomass /
        detritus_biomass_loss(params, 
                              n_pp = params@initial_n_pp, 
                              rates = getRates(params))
}

`detritus_lifetime<-` <- function(params, value) {
    rescale_detritus(params, value / detritus_lifetime(params))
}

carrion_human_origin <- function(params) {
    inflow <- carrion_biomass_inflow(params)
    (inflow[["gear_mort"]] + inflow[["discards"]]) / sum(inflow)
}

`carrion_human_origin<-` <- function(params, value) {
    lifetime <- carrion_lifetime(params)
    inflow <- carrion_biomass_inflow(params)
    human <- inflow[["gear_mort"]] + inflow[["discards"]]
    natural <- inflow[["mu_b"]]
    factor <- (1 / value - 1) * human / natural
    ext_prop <- params@other_params$carrion$ext_prop * factor
    if (ext_prop > 1) {
        warning("I am setting the proportion of human carrion production to the maximal possible value.")
        ext_prop <- 1
    }
    params@other_params$carrion$ext_prop <- ext_prop
    params <- tune_carrion_detritus(params)
    carrion_lifetime(params) <- lifetime
    params
}

rescale_carrion <- function(params, factor) {
    params@initial_n_other[["carrion"]] <- 
        params@initial_n_other[["carrion"]] * factor
    params@species_params$rho_carrion <- 
        params@species_params$rho_carrion / factor
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / factor
    params@other_params$carrion$decompose <-
        params@other_params$carrion$decompose / factor
    params
}

rescale_detritus <- function(params, factor) {
    params@initial_n_pp <- params@initial_n_pp * factor
    params@species_params$interaction_resource <-
        params@species_params$interaction_resource / factor
    params
}

rescaleComponents <- function(params, carrion_factor = 1, detritus_factor = 1) {
    rescale_carrion(rescale_detritus(params, detritus_factor),
                    carrion_factor)
}

tune_carrion_detritus <- function(params) {
    # carrion
    params@other_params$carrion$decompose <- 0
    cin <- sum(carrion_biomass_inflow(params)) / params@initial_n_other$carrion
    cout <- carrion_loss(params)
    if (cin < cout) {
        stop("There is not enough carrion production.")
    }
    params@other_params$carrion$decompose <- cin - cout
    # detritus
    params@other_params$detritus$external <- 0
    inflow <- sum(detritus_biomass_inflow(params))
    outflow <- detritus_biomass_loss(params)
    params@other_params$detritus$external <- outflow - inflow
    params
}

scaleModel <- function(params, factor) {
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / factor
    mizer::scaleModel(params)
}