source("init.R")
params <- readRDS("params121121.rds")
catch <- readRDS("catch.rds")

sp <- params@species_params %>% 
    select(species, w_min, w_mat, w_inf, k_vb, t0, a, b,
           pred_kernel_type, beta, sigma, kernel_exp, kernel_l_l, kernel_u_l,
           kernel_l_r, kernel_u_r,
           biomass_observed, biomass_cutoff, number_observed, number_cutoff,
           yield_observed,
           gearMort, discard, rho_carrion)
gp <- params@gear_params
rp <- params@resource_params
carrion <- getComponent(params, "carrion")

p <- newMultispeciesParams(sp, gear_params = gp, 
                           interaction = params@interaction,
                           n = 0.7, kappa = rp$kappa,
                           w_pp_cutoff = 1, initial_effort = 1,
                           no_w = 200, min_w_pp = min(params@w_full),
                           max_w = max(params@w))
p <- setComponent(p, "carrion", initial_value = carrion$initial_value, 
                  dynamics_fun = "constant_dynamics",
                  encounter_fun = "encounter_contribution",
                  component_params = list(rho = carrion$component_params$rho))

p@initial_n <- params@initial_n

p <- tuneParams(p, 
                     tabs = c("Spectra", "Abundance", "Growth", "Repro",
                              "Catch", "Diet", "Death", "Rates", "Sim"),
                     controls = c("abundance", "predation", "resource", "fishing", 
                                  "reproduction", "other", "interaction"),
                     catch = catch)

saveRDS(p, file = "params121121_s.rds")
