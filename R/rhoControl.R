#' Controlling the carrion encounter rate in the tuning gadget
#'
#' This control allows the user to adjust `rho_carrion`, the coefficient of
#' the species' encounter rate for carrion, for the selected species.
#'
#' @inheritParams mizerEcopath::abundanceControl
#' @family gadget controls
#' @export
rhoControl <- function(input, output, session, params, params_old, flags, ...) {
    observeEvent(
        list(input$rho_detritus, input$rho_carrion, input$n_detritus),
        {
            p <- params()
            sp <- input$sp
            if (!identical(sp, flags$sp_old_rho)) {
                flags$sp_old_rho <- sp
                return()
            }
            # Update slider min/max so that they are a fixed proportion of the
            # parameter value
            # updateSliderInput(session, "rho_detritus",
            #                   min = 0,
            #                   max = signif(ifelse(input$rho_detritus > 0,
            #                                       input$rho_detritus * 2,
            #                                       0.001), 2))
            updateSliderInput(session, "rho_carrion",
                              min = 0,
                              max = signif(ifelse(input$rho_carrion > 0,
                                                  input$rho_carrion * 2,
                                                  0.001), 2))

            # p@species_params[sp, "rho_detritus"]   <- input$rho_detritus
            # p@species_params[sp, "n_detritus"]   <- input$n_detritus
            p@species_params[sp, "rho_carrion"]   <- input$rho_carrion
            p <- setRho(p)
            mizerEcopath::tuneParams_update_species(sp, p, params, params_old)
        },
        ignoreInit = TRUE)
}

#' @rdname rhoControl
#' @inheritParams mizerEcopath::abundanceControlUI
#' @export
rhoControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    tagList(
        tags$h3(tags$a(id = "rho"), "rho"),
        # sliderInput("rho_detritus", "rho_detritus", value = sp$rho_detritus,
        #             min = 0,
        #             max = signif(ifelse(sp$rho_detritus > 0,
        #                                 sp$rho_detritus * 2,
        #                                 0.001), 2)),
        # sliderInput("n_detritus", "n_detritus", value = sp$n_detritus,
        #             min = -.5,
        #             max = .75, step = .05),
        sliderInput("rho_carrion", "rho_carrion", value = sp$rho_carrion,
                    min = 0,
                    max = signif(ifelse(sp$rho_carrion > 0,
                                        sp$rho_carrion * 2,
                                        0.001), 2))
    )
}

#' Set the carrion encounter rate from the `rho_carrion` species parameter
#'
#' @param params A MizerParams object
#' @return An updated MizerParams object
#' @export
setRho <- function(params) {
    # rho <- t(outer(params@w, params@species_params$n_detritus, "^"))
    # rho <- sweep(rho, 1, params@species_params$rho_detritus, "*")
    # # rho[, params@w > 0.01] <- 0
    # params@other_params$detritus$rho <- rho

    params@other_params$carrion$rho <-
        outer(params@species_params$rho_carrion,
              params@w^params@resource_params$n)
    params
}
