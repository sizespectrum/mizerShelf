
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


plot_diff <- function(x1, x2) {
    wc <- compare(x1, x2, max_diffs = Inf)
    if (length(wc)) {
        barplot((x1 - x2) / x2, names.arg = 1:length(x1), horiz = TRUE)
    }
    wc
}

l <- function(w) {
    (w / sp$a) ^ (1 / sp$b)
}

w <- function(l) {
    sp$a * l ^ sp$b
}
