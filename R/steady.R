# Overwrite mizer's steady() function to also set the detritus and carrion
# to steady state

new_steady <- function(params, t_max = 100, t_per = 1.5, dt = 0.1,
         tol = 0.1 * dt, return_sim = FALSE, 
         preserve = c("reproduction_level", "erepro", "R_max"),
         progress_bar = TRUE) {
    params <- validParams(params)
    
    preserve <- match.arg(preserve)
    old_reproduction_level <- getReproductionLevel(params)
    old_R_max <- params@species_params$R_max
    old_erepro <- params@species_params$erepro
    
    # Force the reproduction to stay at the current level
    params@species_params$constant_reproduction <- getRDD(params)
    old_rdd_fun <- params@rates_funcs$RDD
    params@rates_funcs$RDD <- "constantRDD"
    
    # Force other components to stay at current level
    old_other_dynamics <- params@other_dynamics
    for (res in names(params@other_dynamics)) {
        params@other_dynamics[[res]] <- "constant_other"
    }
    
    object <- projectToSteady(params,
                              distance_func = distanceMaxRelRDI,
                              t_per = t_per,
                              t_max = t_max,
                              dt = dt,
                              tol = tol,
                              return_sim = return_sim,
                              progress_bar = progress_bar)
    if (return_sim) {
        params <- object@params
    } else {
        params <- object
    }
    # Restore original RDD and other dynamics
    params@rates_funcs$RDD <- old_rdd_fun
    params@other_dynamics <- old_other_dynamics
    params@species_params$constant_reproduction <- NULL
    
    # bring carrion and detritus back into steady state
    n <- params@initial_n
    n_pp <- params@initial_n_pp
    n_other <- params@initial_n_other
    rates <- getRates(params)
    
    params <- tune_carrion_detritus(params)
    
    if (preserve == "reproduction_level") {
        params <- setBevertonHolt(params, 
                                  reproduction_level = old_reproduction_level)
    } else if (preserve == "R_max") {
        params <- setBevertonHolt(params, 
                                  R_max = old_R_max)
    } else {
        params <- setBevertonHolt(params, erepro = old_erepro)
    }
    
    if (return_sim) {
        object@params <- params
        return(object)
    } else {
        params@time_modified <- lubridate::now()
        return(params)
    }
}

environment(new_steady) <- asNamespace('mizerExperimental')
utils::assignInNamespace("steady", new_steady, ns = "mizerExperimental")