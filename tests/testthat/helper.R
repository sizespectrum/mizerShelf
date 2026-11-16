delayedAssign("shelf_small", {
    sp <- mizer::NS_species_params[c(1, 4, 11), ]
    sp$gear_mort <- 0.1
    sp$discard <- 0.05
    sp$interaction_carrion <- 1e-4
    suppressMessages(newDetritusCarrionParams(
        sp, no_w = 20, w_min_detritus = 1e-6, w_max_detritus = 1,
        info_level = 0
    ))
})
