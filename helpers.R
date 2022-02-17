
# rescale unstructured components
rescaleComponents <- function(params, carrion_factor = 1, detritus_factor = 1) {
    # carrion
    params@initial_n_other[["carrion"]] <- 
        params@initial_n_other[["carrion"]] * carrion_factor
    params@species_params$rho_carrion <- 
        params@species_params$rho_carrion / carrion_factor
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / carrion_factor
    # detritus
    params@initial_n_pp <- params@initial_n_pp * detritus_factor
    params@species_params$interaction_resource <-
        params@species_params$interaction_resource / detritus_factor
    
    params
}

# Set lifetimes by rescaling components
setLifetimes <- function(params, carrion_lifetime, detritus_lifetime) {
    n <- params@initial_n
    n_pp <- params@initial_n_pp
    rates <- getRates(params)
    # carrion
    current_lifetime <- params@initial_n_other$carrion / 
        carrion_loss(params, n = n, rates = rates)
    carrion_factor <- carrion_lifetime / current_lifetime
    # detritus
    current_biomass <- 
        sum(n_pp * params@dw_full * params@w_full)
    current_lifetime <- current_biomass /
        detritus_biomass_loss(params, n_pp, rates)
    detritus_factor <- detritus_lifetime / current_lifetime
    # rescale
    rescaleComponents(params, carrion_factor = carrion_factor,
                 detritus_factor = detritus_factor)
}

scaleModel <- function(params, factor) {
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / factor
    mizer::scaleModel(params)
}


plot_diff <- function(x1, x2) {
    wc <- compare(x1, x2, max_diffs = Inf)
    if (length(wc)) {
        barplot(rel_diff(x1, x2), names.arg = 1:length(x1), horiz = TRUE)
    }
    wc
}

rel_diff <- function(x1, x2) {
    r <- abs((x1 - x2) / (x1 + x2))
    r[is.nan(r)] <- 0
    r
}

l <- function(w) {
    (w / sp$a) ^ (1 / sp$b)
}

w <- function(l) {
    sp$a * l ^ sp$b
}
