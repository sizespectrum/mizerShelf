rhoControlUI <- function(p, sp) {
    tagList(
        tags$h3(tags$a(id = "rho"), "rho"),
        sliderInput("rho_detritus", "rho_detritus", value = sp$rho_detritus,
                    min = 0,
                    max = signif(ifelse(sp$rho_detritus > 0,
                                        sp$rho_detritus * 2,
                                        0.001), 2)),
        sliderInput("rho_carrion", "rho_carrion", value = sp$rho_carrion,
                    min = 0,
                    max = signif(ifelse(sp$rho_carrion > 0,
                                        sp$rho_carrion * 2,
                                        0.001), 2))
    )
}

rhoControl <- function(input, output, session, params, flags, ...) {
    observeEvent(
        list(input$rho_detritus, input$rho_carrion),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_rho)) {
                flags$sp_old_rho <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            updateSliderInput(session, "rho_detritus",
                              min = 0,
                              max = signif(ifelse(input$rho_detritus > 0,
                                                  input$rho_detritus * 2,
                                                  0.001), 2))
            updateSliderInput(session, "rho_carrion",
                              min = 0,
                              max = signif(ifelse(input$rho_carrion > 0,
                                                  input$rho_carrion * 2,
                                                  0.001), 2))
            
            p@species_params[sp, "rho_detritus"]   <- input$rho_detritus
            p@species_params[sp, "rho_carrion"]   <- input$rho_carrion
            p <- setRho(p)
            tuneParams_update_species(sp, p, params)
        },
        ignoreInit = TRUE)
}

setRho <- function(params) {
    params@other_params$detritus$rho <-
        outer(params@species_params$rho_detritus,
              params@w^params@resource_params$n)
    params@other_params$carrion$rho <-
        outer(params@species_params$rho_carrion,
              params@w^params@resource_params$n)
    params
}
