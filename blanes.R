encounter_contribution <- function(params, n_other, component, ...) {
    params@other_params[[component]]$rho * n_other[[component]]
}

constant_dynamics <- function(params, n_other, component, ...) {
    n_other[[component]]
}
