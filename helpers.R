
# rescale unstructured components
rescaleComponents <- function(params, carrion_factor = 1, detritus_factor = 1) {
    params@initial_n_other[["carrion"]] <- 
        params@initial_n_other[["carrion"]] * carrion_factor
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / carrion_factor
    
    params@initial_n_other[["detritus"]] <- 
        params@initial_n_other[["detritus"]] * detritus_factor
    params@other_params[["detritus"]]$rho <- 
        params@other_params[["detritus"]]$rho / detritus_factor
    
    params
}

scaleModel <- function(params, factor) {
    params@other_params[["carrion"]]$rho <- 
        params@other_params[["carrion"]]$rho / factor
    params@other_params[["detritus"]]$rho <- 
        params@other_params[["detritus"]]$rho / factor
    mizer::scaleModel(params)
}