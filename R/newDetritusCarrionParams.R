#' Create new mizer model with detritus and carrion components
#' 
#' @param species_params A species parameter data frame
#' @param w_min_detritus Minimum size of detritus in grams
#' @param w_max_detritus Maximum size of detritus in grams
#' @param n Growth exponent (also used as metabolic exponent p)
#' @param ... Extra parameters to be passed to [newMultispeciesParams()]
#' @return A MizerParams object
#' @export
newDetritusCarrionParams <- function(
        species_params,
        w_min_detritus = NA, w_max_detritus = 1,
        n = 0.7, ...) {
    
    params <- newMultispeciesParams(
        species_params = species_params,
        min_w_pp = w_min_detritus,
        w_pp_cutoff = w_max_detritus,
        n = n, p = n, 
        resource_dynamics = "detritus_dynamics",
        ...)
    
    # Determine the necessary carrion encounter rates so that at maximum
    # size the species have feeding level f0
    f0 <- set_species_param_default(params@species_params, "f0", 0.6)$f0
    ic <- set_species_param_default(params@species_params, "interaction_carrion", 1)$interaction_carrion
    E <- getEncounter(params)[, length(params@w)] /
        (params@w[length(params@w)] ^ n)
    rho <- pmax(0, f0 * params@species_params$h / (1 - f0) - E) * ic
    params@species_params$rho_carrion <- rho
    rho <- outer(params@species_params$rho_carrion, params@w ^ n)
        
    params <- setRateFunction(params, "Mort", "seMort")
    
    params <- setComponent(
        params, "carrion", initial_value = 1,
        dynamics_fun = "carrion_dynamics",
        encounter_fun = "encounter_contribution",
        component_params = list(rho = rho))
    
    params
}
