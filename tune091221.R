source("init.R")
params <- readRDS("params071221.rds")
catch <- readRDS("catch.rds")

rates <- getRates(params)
n <- params@initial_n

component <- "carrion"
loss <- getLoss(params, n, rates, component)
inflow <- getInflow(params, n, rates, component)

sum((params@mu_b * n) %*% 
        (params@w * params@dw))

sum((gearMort(params, rates$f_mort) * n) %*% 
        (params@w * params@dw))

sum(((rates$f_mort * n) %*% (params@w * params@dw)) *
            params@species_params$discard)

params <- tuneParams(params, 
                     tabs = c("Spectra", "Abundance", "Growth", "Repro",
                              "Catch", "Diet", "Death", "Resource", "Sim"),
                     controls = c("abundance", "predation", "rho", "resource", "fishing", 
                                  "reproduction", "other", "interaction"),
                     catch = catch)

saveRDS(params, file = "params091221.rds")
