#' Contribution of unstructured components to the encounter rate
#' 
#' The encounter rate \eqn{E_i(w)} for an unstructured resource like for example
#' carrion is proportional to the total biomass \eqn{B} with a coefficient
#' \eqn{\rhi_i(w)} that depends on the predator species \eqn{i} and the size of
#' the predator:
#' \deqn{E_i(w) = \rho_i(w) B.}
#' 
#' The coefficient \eqn{\rhi_i(w)} is stored as a matrix (species x size) in
#' the `rho` parameter of the component. It has units 1/year.
#' 
#' @param params MizerParams
#' @param n_other Biomasses of unstructured components
#' @param component Name of component whose contribution is requested
#' @param ... Unused
#'
#' @return Array (species x size) with the encounter rate in g/year.
#' @export
encounter_contribution <- function(params, n_other, component, ...) {
    params@other_params[[component]]$rho * n_other[[component]]
}

#' Carrion biomass
#' 
#' @param params MizerParams
#' @return The carrion biomass in grams
#' @export
carrion_biomass <- function(params) {
    params@initial_n_other$carrion
}

#' Carrion dynamics
#'
#' Calculates the carrion biomass at the next timestep from the current
#' carrion biomass.
#'
#' The time evolution of the carrion biomass \eqn{B} is
#' described by
#' \deqn{dB/dt = \tt{production} - \tt{consumption} * B}{dB/dt = production - consumption * B}
#' where
#' * `consumption` is the mass-specific rate of consumption due to consumption
#'   calculated with `carrion_consumption_ms()`,
#' * `production` is the rate at which the rest of the system produces carrion
#'   biomass, calculated with `getCarrionProduction()`.
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
#' @param n_other Other dynamic components. Only `n_other$carrion` is used.
#' @param rates A list of rates as returned by [getRates()]
#' @param dt Time step size
#' @param ... Unused
#'
#' @return A single number giving the carrion biomass at next time step
#' @seealso [detritus_dynamics()]
#' @export
carrion_dynamics <-
    function(params, n, n_other, rates, dt, ...) {
        
        consumption <- carrion_consumption_ms(params, n, rates)
        production <- sum(getCarrionProduction(params, n, rates))
        
        if (consumption) {
            et <- exp(-consumption * dt)
            return(n_other$carrion * et + production / consumption  * (1 - et))
        }
        return(n_other$carrion + production * dt)
    }

#' Mass-specific carrion consumption rate
#' 
#' This includes both the consumption by fish and the decomposition by smaller
#' organisms. 
#' 
#' This mass-specific consumption rate is used in `carrion_dynamics()` to
#' calculate the carrion biomass at the next time step. To get the
#' non-mass-specific consumption rate, use `getCarrionConsumption()`.
#' 
#' The consumption rate by fish is determined by 
#' `other_params(params)$carrion$rho`
#' and the decomposition rate is given by
#' `other_params(params)$carrion$decompose`.
#' 
#' @param params MizerParams
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A number giving the mass-specific consumption rate in grams per year.
#' @export
carrion_consumption_ms <- function(params, n = params@initial_n, 
                         rates = getRates(params)) {
    sum((params@other_params$carrion$rho * n *
             (1 - rates$feeding_level)) %*% 
            params@dw) +
        params@other_params$carrion$decompose
}

#' Get carrion consumption rates
#' 
#' This function returns a named vector with one component for each species
#' giving the rate in grams/year at which that species consumes carrion, as well
#' as a component named `decompose` with the rate in grams/year at which carrion
#' is decomposed by bacteria and other processes. 
#' @param params MizerParams
#' @return A named vector with the consumption rates from all species and
#'   decomposition.
#' @seealso [getCarrionProduction()], [carrion_dynamics()], [getDetritusConsumption()]
#' @export
getCarrionConsumption <- function(params) {
    # consumption by consumers
    feeding_level <- getFeedingLevel(params)
    consumption <- (params@other_params$carrion$rho * params@initial_n *
        (1 - feeding_level)) %*% params@dw
    names(consumption) <- params@species_params$species
    # add decomposition
    consumption <- c(consumption, 
                     decompose = params@other_params$carrion$decompose)
    # Convert from mass specific rate to total rates
    consumption <- consumption * params@initial_n_other$carrion
    
    return(consumption)
}

#' Plot carrion consumption rates
#' 
#' @param params MizerParams
#' @return A pie chart.
#' @export
plotCarrionConsumption <- function(params) {
    consumption <- getCarrionConsumption(params)
    total <- sum(consumption)
    consumption <- consumption[consumption > total/100]
    df <- data.frame(Consumer = names(consumption),
                     Rate = consumption)
    ggplot(df, aes(x = "", y = Rate, fill = Consumer)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Carrion consumption rate [g/year]",
             x = "", y = "")
}

#' Carrion production rate
#' 
#' This is the rate in grams/year at which the rest of the system produces carrion
#' biomass. The production comes from three sources:
#' 
#' 1. animals that have died by natural causes other than predation ("ext_mort"),
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
#' @seealso [getCarrionConsumption()], [carrion_dynamics()], [getDetritusProduction()]
#' @export
getCarrionProduction <- function(params, n = params@initial_n,
                                 rates = getRates(params)) {
    c(ext_mort = sum((params@mu_b * n) %*% (params@w * params@dw)) *
          params@other_params$carrion$ext_prop,
      gear_mort = sum((gearMort(params, rates$f_mort) * n) %*% 
                          (params@w * params@dw)),
      discards = sum(((rates$f_mort * n) %*% (params@w * params@dw)) *
                         params@species_params$discard)
    )
}


#' Plot carrion production rates
#' 
#' @param params MizerParams
#' @return A pie chart.
#' @export
plotCarrionProduction <- function(params) {
    production <- getCarrionProduction(params)
    df <- data.frame(Producer = names(production),
                     Rate = production)
    ggplot(df, aes(x = "", y = Rate, fill = Producer)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Carrion production rate [g/year]",
             x = "", y = "")
}

#' Detritus biomass
#' 
#' The detritus is internally described by a size spectrum in order to reflect
#' the fact that it is available to small predators but becomes unavailable to
#' large predators. The total biomass is thus obtained by integrating over
#' the abundance density multiplied by mass.
#' 
#' @param params MizerParams
#' @param n_pp Detritus spectrum
#' @return The detritus biomass in grams
#' @export
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
#' The time evolution of the detritus biomass \eqn{B} is described by
#' \deqn{dB/dt = \tt{production} - \tt{consumption} * B + \tt{external}}{dB/dt = production - consumption * B + external}
#' where
#' * `consumption` is the mass-specific rate of consumption.
#' * `production` is the rate at which the rest of the system produces 
#'   detritus biomass.
#'
#' The dynamical equation is solved analytically to
#' \deqn{B(t+dt) = B(t)\exp(-\tt{consumption} \cdot dt)
#'   +\frac{\tt{production}}{\tt{consumption}}
#'   (1-\exp(-\tt{consumption} \cdot dt)).}{B(t+dt)
#'   = B(t) exp(-consumption * dt)
#'   +production/consumption * (1 - exp(-consumption * dt)).}
#' This avoids the stability problems that would arise if we used the Euler
#' method to solve the equation numerically.
#'
#' @param params A [MizerParams] object
#' @param n A matrix of current species abundances (species x size)
#' @param n_other Other dynamic components. Only `n_other$carrion` is used.
#' @param n_pp Vector of detritus density
#' @param rates A list of rates as returned by [getRates()]
#' @param dt Time step size
#' @param ... Unused
#'
#' @return A vector giving the detritus spectrum at the next time step.
#' @seealso [carrion_dynamics()]
#' @export
detritus_dynamics <- function(params, n, n_pp, n_other, rates, dt, ...) {
    current_biomass <- detritus_biomass(params, n_pp = n_pp)
    consumption <- detritus_consumption(params, n_pp, rates) / current_biomass
    production <- sum(getDetritusProduction(params, n, n_other, rates))
    
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
#' An internal helper function. This returns the total detritus consumption rate
#' and is used in `detritus_dynamics()` to calculate the detritus abundance at
#' the next time step. To get the consumption rate split up by consumer species,
#' use `getDetritusConsumption()`.
#' 
#' @param params MizerParams
#' @param n_pp Detritus spectrum
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A number giving the consumption rate in grams per year.
#' @export
detritus_consumption <- function(params, n_pp = params@initial_n_pp, 
                                  rates = getRates(params)) {
    sum(rates$resource_mort * n_pp * params@w_full * params@dw_full)
}

#' Get detritus consumption rates by consumer species
#' 
#' This function returns a named vector with one component for each species
#' giving the rate in grams/year at which that species consumes detritus. 
#' @param params MizerParams
#' @return A named vector with the consumption rates from all species
#' @seealso [getDetritusProduction()], [detritus_dynamics()], [getCarrionConsumption()]
#' @export
getDetritusConsumption <- function(params) {
    pred_rate <- getPredRate(params)
    consumption <- sweep(pred_rate, 1, 
                         params@species_params$interaction_resource,
                         "*")
    consumption <- consumption %*% 
        (params@initial_n_pp * params@w_full * params@dw_full)
    
    return(consumption[, 1])
}

#' Plot carrion consumption rates
#' 
#' @param params MizerParams
#' @return A pie chart.
#' @export
plotDetritusConsumption <- function(params) {
    consumption <- getDetritusConsumption(params)
    total <- sum(consumption)
    consumption <- consumption[consumption > total/100]
    df <- data.frame(Consumer = names(consumption),
                     Rate = consumption)
    ggplot(df, aes(x = "", y = Rate, fill = Consumer)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Detritus consumption rate [g/year]",
             x = "", y = "")
}


#' Detritus production rate
#' 
#' Gives a named vector with the rates at which different components of the 
#' ecosystem produce detritus:
#' 1. biomass not assimilated by predators ("feces"),
#' 2. decomposing carrion ("carrion"),  
#' 3. the pelagic zone ("external").
#' 
#' The function returns a vector with the individual contributions. These
#' can be summed with `sum()` to get the total production rate.
#' 
#' @param params MizerParams
#' @param n A matrix of current species abundances (species x size)
#' @param n_other Other dynamic components. Only `n_other$carrion` is used.
#' @param rates A list of rates as returned by [getRates()]
#' 
#' @return A vector with named entries "external",
#' "feces" and "carrion", giving the rates at which carrion biomass
#' is produced by these sources in grams per year.
#' @export
getDetritusProduction <- function(params, n = params@initial_n,
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

#' Plot detritus production rates
#' 
#' @param params MizerParams
#' @return A pie chart.
#' @export
plotDetritusProduction <- function(params) {
    production <- getDetritusProduction(params)
    df <- data.frame(Producer = names(production),
                     Rate = production)
    ggplot(df, aes(x = "", y = Rate, fill = Producer)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Detritus production rate [g/year]",
             x = "", y = "")
}

#' Expected carrion lifetime
#' 
#' The expected carrion lifetime is defined as the inverse of the 
#' mass-specific carrion consumption rate.
#' @param params A MizerParams object
#' @return The number giving the expected lifetime in years.
#' @export
carrion_lifetime <- function(params) {
    1 / carrion_consumption_ms(params)
}

#' @rdname carrion_lifetime
#' @param params A MizerParams object
#' @param value A number with the new value for the expected lifetime in years.
#' 
#' Assigning a new value to the carrion lifetime rescales the carrion
#' abundance while keeping the total consumption and decomposition of carrion 
#' the same (by adjusting the interaction strength of species with carrion
#' and the mass-specific decomposition rate).
#' @export
`carrion_lifetime<-` <- function(params, value) {
    rescale_carrion(params, value / carrion_lifetime(params))
}

#' Expected detritus lifetime
#' 
#' The expected detritus lifetime is defined as the inverse of the 
#' mass-specific detritus consumption rate.
#' @param params A MizerParams object
#' @return The number giving the expected lifetime in years.
#' @export
detritus_lifetime <- function(params) {
    detritus_biomass(params) /
        detritus_consumption(params, 
                              n_pp = params@initial_n_pp, 
                              rates = getRates(params))
}

#' @rdname detritus_lifetime
#' @param params A MizerParams object
#' @param value A number with the new value for the expected lifetime in years
#' 
#' Assigning a new value to the detritus lifetime rescales the detritus
#' abundance while keeping the total consumption of detritus the same (by
#' adjusting the interaction strength of species with detritus).
#' @export
`detritus_lifetime<-` <- function(params, value) {
    rescale_detritus(params, value / detritus_lifetime(params))
}

#' Proportion of carrion production that is of human origin
#'
#' Calculates or sets what proportion of the total carrion production is from
#' either excess gear mortality or from discards.
#'
#' @param params A MizerParams object
#' @export
carrion_human_origin <- function(params) {
    production <- getCarrionProduction(params)
    (production[["gear_mort"]] + production[["discards"]]) / sum(production)
}

#' @export
`carrion_human_origin<-` <- function(params, value) {
    lifetime <- carrion_lifetime(params)
    production <- getCarrionProduction(params)
    human <- production[["gear_mort"]] + production[["discards"]]
    natural <- production[["ext_mort"]]
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

#' Rescale carrion biomass without changing anything else
#' 
#' This multiplies the carrion biomass by a factor and divides the
#' interaction between all species and the carrion by the same
#' factor, so as to keep the total consumption of carrion unchanged.
#' It also divides the mass-specific rate of decomposition by the same
#' factor so that the total carrion decomposition rate stays the same.
#' @param params A MizerParams object
#' @param factor A number
#' @return An updated MizerParams object
#' @export
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

#' Rescale detritus biomass without changing detritus consumption
#' 
#' This multiplies the detritus abundance by a factor and divides the
#' interaction between all species and the detritus by the same
#' factor, so as to keep the total consumption of detritus unchanged.
#' @param params A MizerParams object
#' @param factor A number
#' @return An updated MizerParams object
#' @export
rescale_detritus <- function(params, factor) {
    params@initial_n_pp <- params@initial_n_pp * factor
    params@species_params$interaction_resource <-
        params@species_params$interaction_resource / factor
    params
}

#' @export
rescaleComponents <- function(params, carrion_factor = 1, detritus_factor = 1) {
    rescale_carrion(rescale_detritus(params, detritus_factor),
                    carrion_factor)
}

#' Tune carrion and detritus to steady state
#' 
#' This first sets the rate of decomposition of carrion so that for the given 
#' abundances, the carrion is at steady state. It then sets the rate at which
#' detritus flows in from external sources (e.g. the pelagic zone) so that for
#' the given abundances the detritus is at steady state.
#' 
#' @param params A MizerParams object
#' @return An updated MizerParams object
#' @export
tune_carrion_detritus <- function(params) {
    # carrion
    params@other_params$carrion$decompose <- 0
    cin <- sum(getCarrionProduction(params)) / params@initial_n_other$carrion
    cout <- carrion_consumption_ms(params)
    if (cin < cout) {
        stop("There is not enough carrion production.")
    }
    params@other_params$carrion$decompose <- cin - cout
    # detritus
    params@other_params$detritus$external <- 0
    production <- sum(getDetritusProduction(params))
    outflow <- detritus_consumption(params)
    params@other_params$detritus$external <- outflow - production
    params
}

#' Scale Model Parameters
#'
#' This function scales various model parameters by a given factor.
#'
#' @param params a mizer model object
#' @param factor a numeric value by which to scale the model
#'
#' @return a mizer model object with scaled parameters
#'
#' @export
#'
#' @examples
#' params <- scaleModel(NWMed_params, 0.5)
#'
scaleModel <- function(params, factor) {
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / factor
    params@species_params$rho_carrion <-
        params@species_params$rho_carrion / factor
    params@other_params$detritus$external <-
        params@other_params$detritus$external * factor
    mizer::scaleModel(params, factor)
}

#' @export
constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}