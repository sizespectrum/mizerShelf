#' Contribution of unstructured components to the encounter rate
#' 
#' @param params MizerParams
#' @param n_other Biomasses of unstructured components
#' @param component Name of component whose contribution is requested
#'
#' @return Array species x size
encounter_contribution <- function(params, n_other, component, ...) {
    params@other_params[[component]]$rho * n_other[[component]]
}

#' Carrion biomass
#' 
#' @param params MizerParams
#' @return The carrion biomass in grams
carrion_biomass <- function(params) {
    params@initial_n_other$carrion
}

#' Carrion dynamics
#'
#' Calculates the carrion biomass at the next timestep from the current
#' carrion biomass.
#'
#' The equation for the time evolution of the carrion biomass \eqn{B} is
#' assumed to be of the form
#' \deqn{dB/dt = \tt{production} - \tt{consumption} * B}{dB/dt = production - consumption * B}
#' where
#' * `consumption` is the mass-specific rate of consumption due to consumption, where the
#'   encounter rate is specified by `params@other_params[[component]]$rho`.
#' * `production` is the rate at which the rest of the system produces carrion
#'   biomass. The production comes from three sources: 1) animals
#'   killed by the fishing gear, 2) animals that have died by natural causes
#'   other than predation, 3) discards from fishing.
#'
#' The dynamical equation is solved analytically to
#' \deqn{B(t+dt) = B(t)\exp(-\tt{consumption} \cdot dt)
#'   +\frac{\tt{production}}{\tt{consumption}}
#'   (1-\exp(-\tt{consumption} \cdot dt)).}{B(t+dt)
#'   = B(t) exp(-consumption * dt) + production/consumption * (1 - exp(-consumption * dt)).}
#' This avoids the stability problems that would arise if we used the Euler
#' method to solve the equation numerically.
#'
#' @param params A [MizerParams] object
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' @param dt Time step size
#' @param ... Unused
#'
#' @return A single number giving the component biomass at next time step
#' @export
carrion_dynamics <-
    function(params, n, rates, dt, ...) {
        
        consumption <- carrion_consumption_ms(params, n, rates)
        production <- sum(carrion_production(params, n, rates))
        
        if (consumption) {
            et <- exp(-consumption * dt)
            return(n_other$carrion * et + production / consumption  * (1 - et))
        }
        return(n_other$carrion + production * dt)
    }

#' Mass-specific carrion consumption rate
#' 
#' This includes both the consumption by fish and the decomposition by smaller
#' organisms. The latter rate is given by the parameter
#' `params@other_params$carrion$decompose`.
#' 
#' @param params MizerParams
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A number giving the mass-specific consumption rate in grams per year.
carrion_consumption_ms <- function(params, n = params@initial_n, 
                         rates = getRates(params)) {
    sum((params@other_params$carrion$rho * n *
             (1 - rates$feeding_level)) %*% 
            params@dw) +
        params@other_params$carrion$decompose
}

#' Carrion production rate
#' 
#' This is the rate at which the rest of the system produces carrion
#' biomass. The production comes from three sources:
#' 
#' 1. animals that have died by natural causes other than predation ("external"),
#' 2. animals killed by the fishing gear ("gear_mort"),  
#' 3. discards from fishing ("discards").
#' 
#' The function returns a vector with the individual contributions. These
#' can be summed with `sum()` to get the total production rate.
#' 
#' @param params MizerParams
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A vector with named entries "external",
#' "gear_mort" and "discards", each given the rate at which carrion biomass
#' is produced by these sources in grams per year.
carrion_production <- function(params, n = params@initial_n, 
                                          rates = getRates(params)) {
    c(mu_b = sum((params@mu_b * n) %*% (params@w * params@dw)) *
          params@other_params$carrion$ext_prop,
      gear_mort = sum((gearMort(params, rates$f_mort) * n) %*% 
                          (params@w * params@dw)),
      discards = sum(((rates$f_mort * n) %*% (params@w * params@dw)) *
                         params@species_params$discard)
    )
}

#' Detritus biomass
#' 
#' The detritus is internally described by a size spectrum in order to reflect
#' the fact that it is available to small predators but becomes unavailable to
#' large predators. The total biomass is thus obtained by integrating over
#' the abundance density multiplied by mass.
#' 
#' @param params MizerParams
#' @return The detritus biomass in grams
detritus_biomass <- function(params, n_pp = params@initial_n_pp) {
    sum(n_pp * params@dw_full * params@w_full)
}


#' Detritus dynamics
#'
#' Calculates the detritus size spectrum at the next time step from the current
#' size spectrum. The size spectrum is always held at a power law with the
#' same exponent -- only the intercept is dynamical to reflect the change in
#' total detritus biomass.
#'
#' The equation for the time evolution of the detritus biomass \eqn{B} is
#' assumed to be of the form
#' \deqn{dB/dt = \tt{production} - \tt{consumption} * B + \tt{external}}{dB/dt = production - consumption * B + external}
#' where
#' * `consumption` is the mass-specific rate of consumption due to consumption.
#' * `external` is an influx from external sources. It can be negative in which
#'   case it represents a consumption to external sources. It is specified by
#'   `params@other_params$detritus$external`.
#' * `production` is the rate at which the rest of the system produces biomass that
#'   enters this component. Its calculation depends on the type of component:
#'   + For detritus it comes from feces and is calculated as a proportion 
#'   `params@other_params[["detritus"]]$proportion` of the biomass consumption
#'   rate of all consumers.
#'   + For carrion (corpses) the production comes from three sources: 1) animals
#'   killed by the fishing gear, 2) animals that have died by natural causes
#'   other than predation, 3) discards from fishing.
#'
#' The dynamical equation is solved analytically to
#' \deqn{B(t+dt) = B(t)\exp(-\tt{consumption} \cdot dt)
#'   +\frac{\tt{production} + \tt{external}}{\tt{consumption}}
#'   (1-\exp(-\tt{consumption} \cdot dt)).}{B(t+dt)
#'   = B(t) exp(-consumption * dt)
#'   +(production + external)/(consumption) * (1 - exp(-consumption * dt)).}
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
detritus_dynamics <- function(params, n, n_pp, n_other, rates, dt, ...) {
    current_biomass <- detritus_biomass(params, n_pp = n_pp)
    consumption <- detritus_biomass_consumption(params, n_pp, rates) / current_biomass
    production <- sum(detritus_production(params, n, n_other, rates))
    
    if (consumption) {
        et <- exp(-consumption * dt)
        next_biomass <- current_biomass * et + production / consumption  * (1 - et)
    } else {
        next_biomass <- current_biomass + production * dt
    }
    n_pp * next_biomass / current_biomass
}

#' Detritus consumption rate
#' 
#' @param params MizerParams
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A number giving the consumption rate in grams per year.
detritus_biomass_consumption <- function(params, n_pp = params@initial_n_pp, 
                                  rates = getRates(params)) {
    sum(rates$resource_mort * n_pp * params@w_full * params@dw_full)
}


#' Detritus production rate
#' 
#' This is the rate at which the rest of the system produces detritus
#' biomass. The production comes from three sources:
#' 
#' 1. biomass not assimilated by predators ("feces"),
#' 2. decomposing carrion ("carrion"),  
#' 3. the pelagic zone ("external").
#' 
#' The function returns a vector with the individual contributions. These
#' can be summed with `sum()` to get the total production rate.
#' 
#' @param params MizerParams
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A vector with named entries "external",
#' "feces" and "carrion", each given the rate at which carrion biomass
#' is produced by these sources in grams per year.
detritus_production <- function(params, n = params@initial_n,
                                    n_other = params@initial_n_other,
                                    rates = getRates(params)) {
    consumption <- sweep((1 - rates$feeding_level) * rates$encounter * n, 2,
                         params@dw, "*", check.margin = FALSE)
    feces <- sweep(consumption, 1, (1 - params@species_params$alpha), "*", 
                   check.margin = FALSE)
    carrion <- params@other_params$carrion$decompose * n_other$carrion
    c(external = params@other_params$detritus$external,
      feces = sum(feces),
      carrion = carrion,
    )
}

carrion_lifetime <- function(params) {
    1 / carrion_consumption_ms(params)
}

`carrion_lifetime<-` <- function(params, value) {
    rescale_carrion(params, value / carrion_lifetime(params))
}

detritus_lifetime <- function(params) {
    current_biomass <- 
        sum(params@initial_n_pp * params@dw_full * params@w_full)
    current_biomass /
        detritus_biomass_consumption(params, 
                              n_pp = params@initial_n_pp, 
                              rates = getRates(params))
}

`detritus_lifetime<-` <- function(params, value) {
    rescale_detritus(params, value / detritus_lifetime(params))
}

carrion_human_origin <- function(params) {
    production <- carrion_production(params)
    (production[["gear_mort"]] + production[["discards"]]) / sum(production)
}

`carrion_human_origin<-` <- function(params, value) {
    lifetime <- carrion_lifetime(params)
    production <- carrion_production(params)
    human <- production[["gear_mort"]] + production[["discards"]]
    natural <- production[["mu_b"]]
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
    cin <- sum(carrion_production(params)) / params@initial_n_other$carrion
    cout <- carrion_consumption_ms(params)
    if (cin < cout) {
        stop("There is not enough carrion production.")
    }
    params@other_params$carrion$decompose <- cin - cout
    # detritus
    params@other_params$detritus$external <- 0
    production <- sum(detritus_production(params))
    outflow <- detritus_biomass_consumption(params)
    params@other_params$detritus$external <- outflow - production
    params
}

scaleModel <- function(params, factor) {
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / factor
    mizer::scaleModel(params, factor)
}

constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}