library(shiny)
library(shinyBS)
library(ggplot2)
library(plotly)
library(mizerExperimental)
library(progress)
source("dynamics.R")

fishingControlUI <- function(p, sp) {
    # If there are several gears, we only use the effort for the first.
    # If this is changed by the user, all efforts will be set the same.
    effort <- p@initial_effort[[1]]
    gp <- p@gear_params[p@gear_params$species == sp$species, ]
    if (nrow(gp) != 1) {
        showModal(modalDialog(
            title = "Invalid gear specification",
            HTML(paste0("Currently you can only use models where each ",
                        "species is caught by only one gear. In this model ",
                        input$sp, " is caught by ", nrow(gp), " gears.")),
            easyClose = TRUE
        ))
    }
    # heading
    l1 <- tagList(tags$h4(tags$a(id = sp$species), sp$species))
    # controls depending on selectivity function
    if (gp$sel_func == "knife_edge") {
        l1 <- c(l1, list(
            sliderInput(paste0(sp$species, "_knife_edge_size"),
                        "knife_edge_size",
                        value = gp$knife_edge_size,
                        min = 1,
                        max = signif(gp$knife_edge_size * 2, 2),
                        step = 0.1)))
    } else if (gp$sel_func == "sigmoid_length") {
        l1 <- c(l1, list(
            sliderInput(paste0(sp$species, "_l50"), "L50",
                        value = gp$l50,
                        min = 1,
                        max = signif(gp$l50 * 2, 2),
                        step = 0.1),
            sliderInput(paste0(sp$species, "_ldiff"), "L50-L25",
                        value = gp$l50 - gp$l25,
                        min = 0.1,
                        max = signif(gp$l50 / 4, 2),
                        step = 0.1)))
    } else if (gp$sel_func == "double_sigmoid_length") {
        l1 <- c(l1, list(
            sliderInput(paste0(sp$species, "_l50"), "L50",
                        value = gp$l50,
                        min = 1,
                        max = signif(gp$l50 * 2, 2),
                        step = 0.1),
            sliderInput(paste0(sp$species, "_ldiff"), "L50-L25",
                        value = gp$l50 - gp$l25,
                        min = 0.1,
                        max = signif(gp$l50 / 4, 2),
                        step = 0.1),
            sliderInput(paste0(sp$species, "_l50_right"), "L50 right",
                        value = gp$l50_right,
                        min = 1,
                        max = signif(sp$l50_right * 2, 2),
                        step = 0.1),
            sliderInput(paste0(sp$species, "_ldiff_right"), "L50-L25 right",
                        value = gp$l25_right - gp$l50_right,
                        min = 0.1,
                        max = signif(gp$l50_right / 4, 2),
                        step = 0.1)
        ))
    }
    l1
}

#### Server ####
server <- function(input, output, session) {

    # Load params and create sim
    p_old <- readRDS(file = "params250620.rds")
    sim_old <- project(p_old, t_max = 0.1, t_save = 0.1)

    sp_sel <- c("Hake", "Red mullet", "Angler fish", "Poor cod",
                "Horse mackerel", "Blue whiting")
    sp_idx <- which(p_old@species_params$species %in% sp_sel)
    sp_bgrd <- setdiff(p_old@species_params$species, sp_sel)
    p_old <- markBackground(p_old, species = sp_bgrd)

    # Data frame for yield plot
    ym_old <- data.frame(
        "Year" = rep(c(2020, 2035), each = length(sp_sel)),
        "Species" = rep(sp_sel, times = 2),
        "Yield" = rep(getYield(sim_old)[1, sp_sel], times = 2),
        "Gear" = "Current"
    )

    # Data frame for SSB plot
    bm_old <- data.frame(
        "Year" = rep(c(2020, 2035), each = length(sp_sel)),
        "Species" = rep(sp_sel, times = 2),
        "SSB" = rep(getSSB(sim_old)[1, sp_sel], times = 2),
        "Gear" = "Current"
    )

    # Create controls ####
    output$fishing <- renderUI({
        lapply(sp_sel, function(sp_name) {
            fishingControlUI(p_old, p_old@species_params[sp_name, ])
            })
    })

    # Set params ####
    params <- reactive({
        p <- p_old

        # Set new gears
        for (i in sp_idx) {
            l50_var <- paste0("l50_", p@species_params$species[[i]])
            l25_var <- paste0("l25_", p@species_params$species[[i]])
            p@gear_params$l50[i] <- input[[l50_var]]
            p@gear_params$l25[i] <- input[[l25_var]]
        }

        setFishing(p)
    })

    # Run simulation ####
    sim <- reactive({
        # Create a Progress object
        progress <- shiny::Progress$new(session)
        on.exit(progress$close())

        project(params(), t_max = 15, t_save = 1)#,
                #progress_bar = progress)
    })

    # Plot yield ####
    output$plotYield <- renderPlot({
        y <- getYield(sim())
        ym <- reshape2::melt(y, varnames = c("Year", "Species"),
                             value.name = "Yield")
        ym <- subset(ym, ym$Species %in% sp_sel)
        ym$Gear <- "Modified"
        ym$Year <- ym$Year + 2020
        ym <- rbind(ym_old, ym)
        ggplot(ym) +
            geom_line(aes(x = Year, y = Yield, colour = Species, linetype = Gear)) +
            scale_y_continuous(name="Yield [tonnes/year]", limits = c(0, NA)) +
            scale_colour_manual(values = params()@linecolour) +
            scale_linetype_manual(values = c("Current" = "dotted", "Modified" = "solid")) +
            theme(text = element_text(size = 18))
    })

    # Plot SSB ####
    output$plotSSB <- renderPlot({
        b <- getSSB(sim())[, c(19,17)]
        bm <- reshape2::melt(b, varnames = c("Year", "Species"),
                             value.name = "SSB")
        bm <- subset(bm, bm$Species %in% sp_sel)
        bm$Gear <- "Modified"
        bm$Year <- bm$Year + 2020
        bm <- rbind(bm_old, bm)
        ggplot(bm) +
            geom_line(aes(x = Year, y = SSB, colour = Species, linetype = Gear)) +
            scale_y_continuous(name="SSB [tonnes]", limits = c(0, NA)) +
            scale_colour_manual(values = params()@linecolour) +
            scale_linetype_manual(values = c("Current" = "dotted", "Modified" = "solid")) +
            theme(text = element_text(size = 18))
    })

    # Plot percentage change ####
    output$plotChange <- renderPlot({
        # Plot changes in abundance
        s2 <- sim()
        p <- params()
        year <- input$change_year - 2020
        no_w <- length(p@w)
        w_sel <- seq.int(1, no_w, by = floor(no_w/50))
        w <- p@w[w_sel]
        change <- s2@n[year+1, sp_sel, w_sel]/s2@n[1, sp_sel, w_sel] - 1
        # change_total <- colSums(s2@n[10*year+1, ,w_sel], na.rm = TRUE) /
        #                      colSums(s2@n[1, ,w_sel], na.rm = TRUE) - 1
        # ch <- rbind(change, "Total" = change_total)
        # names(dimnames(ch)) <- names(dimnames(change))
        cf <- reshape2::melt(change)
        cf <- subset(cf, !is.nan(value))
        cf$Species <- as.character(cf$sp)

        # data frame for special points
        w_mat <- p@species_params$w_mat[c(19,17)]
        w50 <- p@species_params$a[c(19,17)] *
            (p@gear_params$l50[c(19,17)])^p@species_params$b[c(19,17)]
        sp <- data.frame("w" = c(w_mat, w50),
                         "y" = c(change[11, which.min(w < w_mat[1])],
                                 change[12, which.min(w < w_mat[2])],
                                 change[11, which.min(w < w50[1])],
                                 change[12, which.min(w < w50[2])]),
                         "Points" = c("Maturity", "Maturity", "L50", "L50"),
                         "Species" = p@species_params$species[c(19,17)])

        ggplot(cf, aes(x = w, y = value)) +
            geom_line(aes(colour = Species, linetype = Species, group = sp)) +
            geom_hline(yintercept = 0) +
            scale_x_log10(name = "Size [g]", labels = prettyNum,
                          breaks = 10^(-3:4)) +
            scale_y_continuous(name = "Percentage change", limits = c(-0.50, 0.60),
                               labels = scales::percent, breaks = (-7:9)/10) +
            scale_colour_manual(values = p@linecolour) +
            scale_linetype_manual(values = p@linetype) +
            theme(text = element_text(size = 14)) +
            geom_point(aes(x = w, y = y, colour = Species, shape = Points),
                       data = sp, size = 3)
    })

    # Plot selectivity curves ####
    output$plotSelectivity <- renderPlot({
        p <- params()
        w_min_idx <- sum(p@w < 0.5)
        w_max_idx <- which.min(p@w < 200)
        w_sel <- seq(w_min_idx, w_max_idx, by = ceiling((w_max_idx-w_min_idx)/50))
        w <- p@w[w_sel]
        selectivity <- p@selectivity[1, sp_sel, w_sel]
        sf <- reshape2::melt(selectivity)
        sf$Gear <- "Modfied"
        selectivity_old <- p_old@selectivity[1, sp_sel, w_sel]
        sf_old <- reshape2::melt(selectivity_old)
        sf_old$Gear <- "Current"
        sf <- rbind(sf, sf_old)
        names(sf)[1] <- "Species"
        sf <- subset(sf, value > 0)
        if (input$selectivity_x == "Weight") {
            gg <- ggplot(sf, aes(x = w, y = value)) +
                geom_line(aes(colour = Species, linetype = Gear)) +
                scale_x_continuous(name = "Size [g]", labels = prettyNum) +
                scale_y_continuous(name = "Selectivity",
                                   labels = scales::percent) +
                scale_colour_manual(values = p@linecolour) +
                theme(text = element_text(size = 18))
        } else {
            a <- p@species_params$a
            names(a) <- p@species_params$species
            b <- p@species_params$b
            names(b) <- p@species_params$species
            gg <- ggplot(sf, aes(x = (w/a[Species])^(1/b[Species]), y = value)) +
                geom_line(aes(colour = Species, linetype = Gear)) +
                scale_x_continuous(name = "Length [cm]", labels = prettyNum) +
                scale_y_continuous(name = "Selectivity",
                                   labels = scales::percent) +
                scale_colour_manual(values = p@linecolour) +
                theme(text = element_text(size = 18))
        }
        gg
    })

    # Plot catch by size ####
    output$plotCatch <- renderPlot({
        year <- input$catch_year - 2020
        s2 <- sim()
        p <- params()
        w_min_idx <- sum(p@w < 4)
        w_max_idx <- which.min(p@w < 200)
        w_sel <- seq(w_min_idx, w_max_idx, by = 1)
        w <- p@w[w_sel]
        catch_old <- p_old@selectivity[1, sp_sel, w_sel] *
            sim_old@n[2, sp_sel, w_sel] * p@initial_effort * rep(w, each = 2)
        catchf_old <- reshape2::melt(catch_old)
        catchf_old$Gear <- "Current"
        catch <- p@selectivity[1, sp_sel, w_sel] * s2@n[year+1, sp_sel,w_sel] *
            p@initial_effort * rep(w, each = 2)
        catchf <- reshape2::melt(catch)
        catchf$Gear <- "Modified"
        catchf <- rbind(catchf, catchf_old)
        names(catchf)[1] <- "Species"
        if (input$catch_x == "Weight") {
            ggplot(catchf, aes(x = w, y = value)) +
                geom_line(aes(colour = Species, linetype = Gear)) +
                scale_x_continuous(name = "Size [g]", labels = prettyNum) +
                scale_y_continuous(name = "Yield distribution") +
                scale_colour_manual(values = p@linecolour) +
                scale_linetype_manual(values = c("Current" = "dotted",
                                                 "Modified" = "solid")) +
                theme(text = element_text(size = 18))
        } else {
            a <- p@species_params$a
            names(a) <- p@species_params$species
            b <- p@species_params$b
            names(b) <- p@species_params$species
            ggplot(catchf, aes(x = (w/a[Species])^(1/b[Species]), y = value)) +
                geom_line(aes(colour = Species, linetype = Gear)) +
                scale_x_continuous(name = "Length [cm]", labels = prettyNum) +
                scale_y_continuous(name = "Yield distribution") +
                scale_colour_manual(values = p@linecolour) +
                scale_linetype_manual(values = c("Current" = "dotted",
                                                 "Modified" = "solid")) +
                theme(text = element_text(size = 18))
        }
    })

    # Plot size spectrum ####
    output$plotSpectrum <- renderPlotly({
        animateSpectra(sim(), resource = FALSE)
    })

    # Constraints on sliders ####
    # L25 must always be smaller than l50
    observe({
        if (input$l50_hake <= input$l25_hake) {
            updateSliderInput(session, "l25_hake", value = input$l50_hake-0.01)
            if (input$l50_hake <= 12) {
                updateSliderInput(session, "l50_hake", value = input$l50_hake+0.01)
            }
        }
    })
    observe({
        if (input$l50_mullet <= input$l25_mullet) {
            updateSliderInput(session, "l25_mullet", value = input$l50_mullet-0.01)
            if (input$l50_mullet <= 12) {
                updateSliderInput(session, "l50_mullet", value = input$l50_mullet+0.01)
            }
        }
    })

}

#### User interface ####
ui <- fluidPage(

    titlePanel("Gear modification: target species and the background ecosystem"),

    sidebarLayout(

        sidebarPanel(
            h3("Modified fishing"),
            # sliderInput("effort", "Effort",
            #             value=0.4, min=0.3, max=0.5),
            # bsTooltip("effort", "Explanation of this slider", "right"),
            uiOutput("fishing")
            # h4("Hake selectivity"),
            # sliderInput("l50_hake", "L50", post = "cm",
            #             value=20.5, min=12, max=22, step = 0.01),
            # sliderInput("l25_hake", "L25", post = "cm",
            #             value=20.1, min=12, max=22, step = 0.01),
            # h4("Mullet selectivity"),
            # sliderInput("l50_mullet", "L50", post = "cm",
            #             value=15.8, min=12, max=22, step = 0.01),
            # sliderInput("l25_mullet", "L25", post = "cm",
            #             value=13.6, min=12, max=22, step = 0.01),
            # img(src = "logo_minouw_blue.png", width = "200px"),
            # actionButton("introBut", "View Introduction again")
        ),  # endsidebarpanel

        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Selectivity",
                    br(),
                    p("A comparison of the selectivity of the current and the
                      modified gear on hake and mullet."),
                    p("You control the selectivity of the modified gear with
                      the sliders on the left."),
                    plotOutput("plotSelectivity"),
                    radioButtons("selectivity_x", "Show size in:",
                                 choices = c("Weight", "Length"),
                                 selected = "Length", inline = TRUE)
                ),
                tabPanel(
                    "Total Catch",
                    br(),
                    p("Yield from modified fishing compared with current fishing
                      over time."),
                    plotOutput("plotYield"),
                    p("With the default settings, the yield of hake is initially
                      below that of the current fishing.  As hake's size
                      structure recovers, its yield increases.  Since the
                      fishing mortality of red mullet remains close to that of
                      current fishing, the decrease in red-mullet yield comes
                      mostly from the change in abundance of hake and of
                      background species.")
                ),
                tabPanel(
                    "Spectrum",
                    plotlyOutput("plotSpectrum")
                ),
                tabPanel(
                    "% Change",
                    br(),
                    p("Percentage change in abundance from 'current' to
                      'modified' gear over time"),
                    wellPanel(
                        sliderInput("change_year", "Year",
                                    value = 2023, min = 2020,
                                    max = 2035, step = 1,
                                    animate = TRUE, sep = "")
                    ),
                    plotOutput("plotChange"),
                    p("With the default settings, the main effects on target
                      species is to increase the abundance of hake, in the short
                      term, and reduce the abundance of red mullet.  In
                      addition, you can see how the change abundance of the
                      target species, changes the background species, which in
                      turn feeds back to the target species.")
                ),
                tabPanel(
                    "SSB",
                    br(),
                    p("Spawning stock biomass over time."),
                    plotOutput("plotSSB")
                ),
                tabPanel(
                    "Catch by Size",
                    br(),
                    p("Biomass catch as a function of size. This shows how the
                      improved selectivity leads to more of the caught biomass
                      to consist of larger fish."),
                    wellPanel(
                        sliderInput("catch_year", "Year",
                                    value = 2023, min = 2020,
                                    max = 2035, step = 1,
                                    animate = TRUE, sep = "")
                    ),
                    plotOutput("plotCatch"),
                    radioButtons("catch_x", "Show size in:",
                                 choices = c("Weight", "Length"),
                                 selected = "Length")
                )
            )

        )  # end mainpanel
    )  # end sidebarlayout
)

shinyApp(ui = ui, server = server) # this launches your app
