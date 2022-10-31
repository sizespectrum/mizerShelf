library(ggplot2)
library(plotly)

#' Plot the biomass of species and unstructured components through time
#'
#' After running a projection, the biomass of each species and each unstructured
#' component can be plotted against time. The biomass is calculated within user
#' defined size limits (min_w, max_w, min_l, max_l, see
#' [get_size_range_array()]).
#'
#' @param sim An object of class \linkS4class{MizerSim}
#' @param species The species to be selected. Optional. By default all target
#'   species are selected. A vector of species names, or a numeric vector with
#'   the species indices, or a logical vector indicating for each species
#'   whether it is to be selected (TRUE) or not.
#' @param y_ticks The approximate number of ticks desired on the y axis.
#' @param start_time The first time to be plotted. Default is the beginning of
#'   the time series.
#' @param end_time The last time to be plotted. Default is the end of the time
#'   series.
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum. Any
#'   values below 1e-20 are always cut off.
#' @param total A boolean value that determines whether the total biomass from
#'   all species is plotted as well. Default is FALSE.
#' @param background A boolean value that determines whether background species
#'   are included. Ignored if the model does not contain background species.
#'   Default is TRUE.
#' @param highlight Name or vector of names of the species to be highlighted.
#' @param return_data A boolean value that determines whether the formatted data
#'   used for the plot is returned instead of the plot itself. Default value is
#'   FALSE
# #' @inheritDotParams get_size_range_array -params
#'
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the four variables 'Year', 'Biomass', 'Species', 'Legend' is
#'   returned.
#'   
#' @import ggplot2
#' @export
#' @family plotting functions
#' @examples
#' \donttest{
#' NWMed_sim <- project(NWMed_params, t_max = 15, effort = 0.6)
#' plotBiomass(NWMed_sim, species = c("Hake", "Red mullet"))
#' plotBiomass(NWMed_sim, start_time = 5, end_time = 15)
#'
#' # Returning the data frame
#' fr <- plotBiomass(NWMed_sim, return_data = TRUE)
#' str(fr)
#' }
plotBiomass <- function(sim, species = NULL, 
                        start_time, end_time, 
                        y_ticks = 6, ylim = c(NA, NA), 
                        total = FALSE, background = TRUE, 
                        highlight = NULL, return_data = FALSE,
                        ...) {
    # If there is no carrion component then call the mizer function
    if (is.null(getComponent(params, "carrion"))) {
        return(mizer::plotBiomass(sim, species = species,
                                  start_time = start_time, end_time = end_time,
                                  y_ticks = y_ticks, ylim = ylim,
                                  total = total, background = background,
                                  highlight = highlight, 
                                  return_data = return_data, ...))
    }
    
    df <- mizer::plotBiomass(sim, species = species,
                             start_time = start_time, end_time = end_time,
                             y_ticks = y_ticks, ylim = ylim,
                             total = total, background = background,
                             highlight = highlight, 
                             return_data = TRUE, ...)
    
    params <- sim@params
    species <- valid_species_arg(sim, species)
    if (missing(start_time)) start_time <- 
        as.numeric(dimnames(sim@n)[[1]][1])
    if (missing(end_time)) end_time <- 
        as.numeric(dimnames(sim@n)[[1]][dim(sim@n)[1]])
    if (start_time >= end_time) {
        stop("start_time must be less than end_time")
    }
    
    # detritus
    d_biomass <- rowSums(sweep(sim@n_pp, 2, params@dw_full * params@w_full, "*"))
    times <- as.numeric(names(d_biomass))
    sel_times <- (times >= start_time) & (times <= end_time)
    br <- data.frame(Year = times[sel_times],
                     Biomass = d_biomass[sel_times],
                     Species = "Detritus")
    # Implement ylim and a minimal cutoff
    min_value <- 1e-20
    br <- br[br$Biomass >= min_value &
                 (is.na(ylim[1]) | br$Biomass >= ylim[1]) &
                 (is.na(ylim[2]) | br$Biomass <= ylim[2]), c(1, 2, 3)]
    br$Legend <- br$Species
    
    # other components
    bc <- unlist(sim@n_other)
    dim(bc) <- dim(sim@n_other)
    dimnames(bc) <- dimnames(sim@n_other)
    times <- as.numeric(dimnames(bc)[[1]])
    bc <- bc[(times >= start_time) & (times <= end_time), , drop = FALSE]
    bc <- melt(bc)
    # Implement ylim and a minimal cutoff and bring columns in desired order
    min_value <- 1e-20
    bc <- bc[bc$value >= min_value &
                 (is.na(ylim[1]) | bc$value >= ylim[1]) &
                 (is.na(ylim[2]) | bc$value <= ylim[2]), c(1, 3, 2)]
    names(bc) <- c("Year", "Biomass", "Species")
    bc$Legend <- bc$Species
    
    plot_dat <- rbind(df, bc, br)
    if (return_data) return(plot_dat) 
    
    plotDataFrame(plot_dat, params, xlab = "Year", ylab = "Biomass [g]",
                  ytrans = "log10", 
                  y_ticks = y_ticks, highlight = highlight,
                  legend_var = "Legend")
}

#' @rdname plotBiomass
#' @export
plotlyBiomass <- function(sim,
                          species = NULL,
                          start_time,
                          end_time,
                          y_ticks = 6,
                          ylim = c(NA, NA),
                          total = FALSE,
                          background = TRUE,
                          highlight = NULL,
                          ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotBiomass", argg),
             tooltip = c("Species", "Year", "Biomass"))
}

#' Plot Death
#' 
#' @import ggplot2
#' @export
plotDeath <- function(object, species = NULL, proportion = TRUE, return_data = FALSE)
{
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    # TODO: Remove this fix once core mizer makes sure a default is set
    if (!"External" %in% names(getColours(params))) {
        params <- setColours(params, c(External = "grey"))
    }
    if (!"Fishing" %in% names(getColours(params))) {
        params <- setColours(params, c(Fishing = "red"))
    }
    if (!"Gear" %in% names(getColours(params))) {
        params <- setColours(params, c(Gear = "brown"))
    }
    
    species <- valid_species_arg(params, species)
    
    pred_rate <- getPredRate(params)
    f_mort <- getFMort(params)
    gear_mort <- gearMort(params, f_mort)
    mort <- getMort(params)
    plot_dat <- NULL
    for (iSpecies in species)
    {
        fish_idx_full <- (params@w_full >= params@species_params[iSpecies, "w_min"]) &
            (params@w_full <= params@species_params[iSpecies, "w_inf"])
        fish_idx <- (params@w >= params@species_params[iSpecies, "w_min"]) &
            (params@w <= params@species_params[iSpecies, "w_inf"])
        predation <- params@interaction[, iSpecies] *
            pred_rate[, fish_idx_full]
        fishing <- f_mort[iSpecies, fish_idx]
        external <- ext_mort(params)[iSpecies, fish_idx]
        gear <- gear_mort[iSpecies, fish_idx]
        total <- mort[iSpecies, fish_idx]
        ylab <- "Death rate [1/year]"
        if (proportion) {
            predation <- predation / rep(total, each = dim(predation)[[1]])
            external <- external / total
            fishing <- fishing / total
            gear <- gear / total
            ylab <- "Proportion of all death"
        }
        # Make data.frame for plot
        plot_dat <-
            rbind(plot_dat,
                  data.frame(w = params@w[fish_idx],
                             value = external,
                             Cause = "External",
                             Prey = iSpecies),
                  data.frame(w = params@w[fish_idx],
                             value = fishing,
                             Cause = "Fishing",
                             Prey = iSpecies),
                  data.frame(w = params@w[fish_idx],
                             value = gear,
                             Cause = "Gear",
                             Prey = iSpecies),
                  data.frame(w = rep(params@w[fish_idx], each = dim(predation)[[1]]),
                             value = c(predation),
                             Cause = params@species_params$species,
                             Prey = iSpecies)
            )
    }
    
    if (return_data) return(plot_dat)
    
    plotDataFrame(plot_dat, params, style = "area", xtrans = "log10", wrap_var = "Prey",
                  wrap_scale = "free_x", xlab = "Size [g]", ylab = ylab)
}


#' @rdname plotDeath
#' @importFrom plotly ggplotly
#' @export
plotlyDeath <- function(object,
                        species = NULL,
                        proportion = TRUE,
                        ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotDeath", argg),
             tooltip = c("value", "Cause", "w"))
}

#' @export
plotYieldMinusDiscards <- function(sim, sim2,
                      species = NULL,
                      total = FALSE, log = TRUE,
                      highlight = NULL, return_data = FALSE,
                      ...) {
    params <- sim@params
    species <- valid_species_arg(sim, species)
    if (missing(sim2)) {
        y <- getYield(sim, ...)
        y <- sweep(y, 2, 1 - sim@params@species_params$discard, "*")
        y_total <- rowSums(y)
        y <- y[, (as.character(dimnames(y)[[2]]) %in% species),
               drop = FALSE]
        if (total) {
            # Include total
            y <- cbind(y, "Total" = y_total)
        }
        plot_dat <- melt(y, varnames = c("Year", "Species"),
                         value.name = "Yield")
        plot_dat <- subset(plot_dat, plot_dat$Yield > 0)
        # plotDataFrame() needs the columns in a particular order
        plot_dat <- plot_dat[, c(1, 3, 2)]
        
        if (nrow(plot_dat) == 0) {
            warning("There is no yield to include.")
        }
        if (return_data) return(plot_dat)
        
        plotDataFrame(plot_dat, params,
                      ylab = "Yield [g/year]",
                      ytrans = ifelse(log, "log10", "identity"),
                      highlight = highlight)
    } else {
        # We need to combine two plots
        if (!all(dimnames(sim@n)$time == dimnames(sim2@n)$time)) {
            stop("The two simulations do not have the same times")
        }
        ym <- plotYield(sim, species = species,
                        total = total, log = log,
                        highlight = highlight, return_data = TRUE, ...)
        ym <- sweep(ym, 2, 1 - sim@params@species_params$discard, "*")
        ym2 <- plotYield(sim2, species = species,
                         total = total, log = log,
                         highlight = highlight, return_data = TRUE, ...)
        ym2 <- sweep(ym2, 2, 1 - sim@params@species_params$discard, "*")
        ym$Simulation <- rep(1, nrow(ym)) # We don't use recycling because that
        # fails when there are zero rows.
        ym2$Simulation <- rep(2, nrow(ym2))
        ym <- rbind(ym, ym2)
        
        if (return_data) return(ym)
        
        plotDataFrame(ym, params,
                      ylab = "Yield [g/year]",
                      ytrans = ifelse(log, "log10", "identity"),
                      highlight = highlight, wrap_var = "Simulation")
    }
}