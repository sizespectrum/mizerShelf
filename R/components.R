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

# Callbacks receive the complete effective rate context. Selectively rebuilding
# absent entries can bypass full-rate providers and is no longer supported.
require_shelf_rates <- function(rates, need) {
    missing <- need[vapply(need, function(field) {
        is.null(rates[[field]])
    }, logical(1))]
    if (length(missing)) {
        stop("Shelf calculations need the complete getRates() context; ",
             "missing ",
             paste(missing, collapse = ", "), ".")
    }
    rates
}

# Integral over the consumer size spectrum, one value per species.
#
# Calculates \eqn{\int N_i(w) K_i(w)\,dw} the way [mizer::sizeIntegral()] does,
# so that it uses the quadrature scheme the model is actually on. It is gated
# on `bin_average_weight()`, which returns the weighting unchanged unless
# second-order bin-averaging is switched on, so nothing changes for a model on
# the default scheme.
#
# `weighting` must be a species x size matrix.
size_integral <- function(params, n, weighting) {
    drop((bin_average_weight(weighting, params) * n) %*% params@dw)
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
    rates <- require_shelf_rates(rates, "feeding_level")
    sum(size_integral(params, n, params@other_params$carrion$rho *
                          (1 - rates$feeding_level))) +
        params@other_params$carrion$decompose
}

#' Get carrion consumption rates
#'
#' Returns a named vector with one entry for each species giving the rate in
#' grams/year at which that species consumes carrion, plus a `decompose` entry
#' for bacterial decomposition.
#'
#' @param params A [MizerParams] object.
#' @param ... Unused
#' @return A named vector with the consumption rates from all species and
#'   decomposition.
#' @seealso [getCarrionProduction()], [carrion_dynamics()], [getDetritusConsumption()]
#' @export
getCarrionConsumption <- function(params, ...) {
    feeding_level <- getRates(params)$feeding_level
    consumption <- size_integral(params, params@initial_n,
                                 params@other_params$carrion$rho *
                                     (1 - feeding_level))
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
#' @examples
#' plotCarrionConsumption(NWMed_params)
#' @export
plotCarrionConsumption <- function(params) {
    consumption <- getCarrionConsumption(params)
    total <- sum(consumption)
    consumption <- consumption[consumption > total/100]
    df <- data.frame(Consumer = names(consumption),
                     Rate = consumption)
    ggplot(df, aes(x = "", y = .data$Rate, fill = .data$Consumer)) +
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
#' @param params A [MizerParams] object.
#' @param n A matrix of current species abundances (species x size)
#' @param rates A list of rates as returned by [getRates()]
#' @param ... Unused
#'
#' @return A vector with named entries "external",
#' "gear_mort" and "discards", each given the rate at which carrion biomass
#' is produced by these sources in grams per year.
#' @seealso [getCarrionConsumption()], [carrion_dynamics()], [getDetritusProduction()]
#' @export
getCarrionProduction <- function(params, n = params@initial_n,
                                 rates = getRates(params), ...) {
    rates <- require_shelf_rates(rates, "f_mort")
    # Each contribution is the biomass killed per year, so each weighting
    # carries a factor of the body weight.
    biomass_killed <- function(mort) {
        size_integral(params, n, sweep(mort, 2, params@w, "*",
                                       check.margin = FALSE))
    }
    c(ext_mort = sum(biomass_killed(params@mu_b)) *
          params@other_params$carrion$ext_prop,
      gear_mort = sum(biomass_killed(gearMort(params, rates$f_mort))),
      discards = sum(biomass_killed(rates$f_mort) *
                         params@species_params$discard)
    )
}


#' Plot carrion production rates
#'
#' @param params MizerParams
#' @return A pie chart.
#' @examples
#' plotCarrionProduction(NWMed_params)
#' @export
plotCarrionProduction <- function(params) {
    production <- getCarrionProduction(params)
    df <- data.frame(Producer = names(production),
                     Rate = production)
    ggplot(df, aes(x = "", y = .data$Rate, fill = .data$Producer)) +
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
    sum(n_pp * params@dw_full * bin_average_weight(params@w_full, params))
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
    if (!is.finite(current_biomass) || current_biomass <= 0) {
        stop("Detritus dynamics require a positive biomass and stored shape.")
    }
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
    rates <- require_shelf_rates(rates, "resource_mort")
    sum(n_pp * params@dw_full *
            bin_average_weight(rates$resource_mort * params@w_full, params))
}

#' Get detritus consumption rates by consumer species
#'
#' Returns a named vector with one entry for each species giving the rate in
#' grams/year at which that species consumes detritus.
#'
#' @param params A [MizerParams] object.
#' @param ... Unused
#' @return A named vector with the consumption rates from all species
#' @seealso [getDetritusProduction()], [detritus_dynamics()], [getCarrionConsumption()]
#' @export
getDetritusConsumption <- function(params, ...) {
    pred_rate <- getRates(params)$pred_rate
    consumption <- sweep(pred_rate, 1,
                         params@species_params$interaction_resource,
                         "*")
    weighting <- sweep(consumption, 2, params@w_full, "*")
    consumption <- bin_average_weight(weighting, params) %*%
        (params@initial_n_pp * params@dw_full)

    return(consumption[, 1])
}

#' Plot detritus consumption rates
#'
#' @param params MizerParams
#' @return A pie chart.
#' @examples
#' plotDetritusConsumption(NWMed_params)
#' @export
plotDetritusConsumption <- function(params) {
    consumption <- getDetritusConsumption(params)
    total <- sum(consumption)
    consumption <- consumption[consumption > total/100]
    df <- data.frame(Consumer = names(consumption),
                     Rate = consumption)
    ggplot(df, aes(x = "", y = .data$Rate, fill = .data$Consumer)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Detritus consumption rate [g/year]",
             x = "", y = "")
}


#' Detritus production rate
#'
#' Returns a named vector with the rates at which different components of the
#' ecosystem produce detritus:
#' 1. biomass not assimilated by predators ("feces"),
#' 2. decomposing carrion ("carrion"),
#' 3. the pelagic zone ("external").
#'
#' The function returns a vector with the individual contributions. These
#' can be summed with `sum()` to get the total production rate.
#'
#' @param params A [MizerParams] object.
#' @param n A matrix of current species abundances (species x size)
#' @param n_other Other dynamic components. Only `n_other$carrion` is used.
#' @param rates A list of rates as returned by [getRates()]
#' @param ... Unused
#'
#' @return A vector with named entries "external",
#' "feces" and "carrion", giving the rates at which carrion biomass
#' is produced by these sources in grams per year.
#' @export
getDetritusProduction <- function(params, n = params@initial_n,
                                  n_other = params@initial_n_other,
                                  rates = getRates(params), ...) {
    rates <- require_shelf_rates(rates, c("encounter", "feeding_level"))
    feces <- size_integral(
        params, n,
        sweep((1 - rates$feeding_level) * rates$encounter, 1,
              1 - params@species_params$alpha, "*", check.margin = FALSE))
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
#' @examples
#' plotDetritusProduction(NWMed_params)
#' @export
plotDetritusProduction <- function(params) {
    production <- getDetritusProduction(params)
    df <- data.frame(Producer = names(production),
                     Rate = production)
    ggplot(df, aes(x = "", y = .data$Rate, fill = .data$Producer)) +
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

#' @param value The desired proportion of carrion production that is of
#'   human origin.
#' @rdname carrion_human_origin
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

#' Rescale carrion and detritus biomass without changing anything else
#'
#' A convenience wrapper that calls [rescale_carrion()] and
#' [rescale_detritus()].
#'
#' @param params A MizerParams object
#' @param carrion_factor A number by which to multiply the carrion biomass
#' @param detritus_factor A number by which to multiply the detritus biomass
#' @return An updated MizerParams object
#' @export
rescaleComponents <- function(params, carrion_factor = 1, detritus_factor = 1) {
    rescale_carrion(rescale_detritus(params, detritus_factor),
                    carrion_factor)
}

#' Tune carrion and detritus to steady state
#'
#' This standalone helper supports the known Shelf providers with carrion as
#' the only component. For composed or modified models, use [tuneSteadyState()]
#' with declared coordinated strategies so all affected equations are checked.
#'
#' This first sets the rate of decomposition of carrion so that for the given
#' abundances, the carrion is at steady state. It then sets the rate at which
#' detritus flows in from external sources (e.g. the pelagic zone) so that for
#' the given abundances the detritus is at steady state.
#'
#' Note that this reaches a steady state by changing *parameters* — the carrion
#' decomposition rate and the external detritus inflow — so that the stored
#' abundances become a fixed point. That is what is needed after a steady-state
#' finder that holds the components fixed, and it is why [tuneSteadyState()]
#' and [steady()] use the declared joint shelf tuning strategy:
#'
#' * [tuneSteadyState()] and [steady()] hold the carrion at its stored value
#'   while they work, then the declared balancer adjusts both budgets. They
#'   leave the components exactly at a fixed point.
#' * [mizer::findSteadyState()] with the default `solver = "project"`, and
#'   [mizer::projectUntilSettled()], advance the carrion like every other state
#'   variable, so they settle it themselves, to within their convergence
#'   tolerance, and leave the parameters alone. They have no mizerShelf method
#'   and do not need one. Calling this function after one of them would
#'   re-solve for parameters the model has just been settled against.
#' * [mizer::findSteadyState()] with `solver = "newton"` declines a model with
#'   custom resource dynamics, which includes every mizerShelf model, and says
#'   so.
#'
#' @param params A MizerParams object
#' @return An updated MizerParams object
#' @seealso [tuneSteadyState()], [carrion_lifetime()], [detritus_lifetime()]
#' @export
tune_carrion_detritus <- function(params) {
    params <- validParams(params)
    standard <- identical(params$resource_dynamics, "detritus_dynamics") &&
        identical(params$other_dynamics$carrion, "carrion_dynamics") &&
        identical(params$other_encounter$carrion, "encounter_contribution")
    if (!standard || !shelf_known_rates(params) ||
            !setequal(names(params$initial_n_other), "carrion")) {
        stop("Standalone shelf balancing requires the known Shelf providers ",
             "with carrion as the only component. Use tuneSteadyState() with ",
             "declared coordinated strategies for composed or modified models.")
    }
    rates <- getRates(params)
    state <- list(n = params$initial_n, n_pp = params$initial_n_pp,
                  n_other = params$initial_n_other)
    targets <- shelf_tuning_targets(params, state$n, state$n_pp, state$n_other,
                                    rates)
    result <- shelf_balance(params, state$n, state$n_pp, state$n_other, rates,
                            targets)
    actual_rates <- getRates(result)
    actual <- shelf_equation_rates(result, state$n, state$n_pp, state$n_other,
                                   actual_rates)
    equal <- function(x, y) {
        if (is.list(x) || is.list(y)) {
            if (!is.list(x) || !is.list(y) || !identical(names(x), names(y)) ||
                    length(x) != length(y)) return(FALSE)
            return(all(vapply(seq_along(x), function(i) {
                equal(x[[i]], y[[i]])
            }, logical(1))))
        }
        if (!is.numeric(x) || !is.numeric(y) ||
                !identical(dim(x), dim(y)) ||
                !identical(dimnames(x), dimnames(y)) ||
                !identical(names(x), names(y)) || length(x) != length(y)) {
            return(FALSE)
        }
        all(is.finite(x)) && all(is.finite(y)) &&
            all(abs(x - y) <= 1e-12 + 1e-8 * abs(x))
    }
    if (!equal(actual, targets) ||
            !equal(shelf_consumer_rates(params, rates),
                   shelf_consumer_rates(result, actual_rates)) ||
            !equal(state, list(n = result$initial_n, n_pp = result$initial_n_pp,
                               n_other = result$initial_n_other))) {
        stop("Shelf balancing did not preserve the captured state and rates.")
    }
    result
}

#' Keep a component's abundance constant
#'
#' A dynamics function for use as `other_dynamics` for a component that is
#' held at its current value, i.e. it never changes over time.
#'
#' @param params A MizerParams object
#' @param n_other A list with the current biomasses/abundances of the other
#'   components
#' @param component The name of the component whose abundance is returned
#'   unchanged
#' @param ... Unused
#' @return The unchanged abundance of `component`
#' @export
constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}
