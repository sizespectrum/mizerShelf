#' Plot death rates
#'
#' @param object A MizerSim or MizerParams object.
#' @param species A character vector of species to plot. If NULL, all species in
#'   the model will be plotted.
#' @param proportion A logical value indicating whether to plot death rates as
#'   proportions of total mortality.
#' @param return_data A logical value indicating whether to return the data used
#'   to plot.
#' @return If \code{return_data = TRUE}, a data frame with the values used to
#'   plot.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' plotDeath(NWMed_params, species = "Hake")
#' }
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
            (params@w_full <= params@species_params[iSpecies, "w_max"])
        fish_idx <- (params@w >= params@species_params[iSpecies, "w_min"]) &
            (params@w <= params@species_params[iSpecies, "w_max"])
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


#' Plot yield minus discards
#'
#' @param sim An \code{\link{MizerSim}} object.
#' @param sim2 Optional second \code{\link{MizerSim}} object.
#' @param species Species to plot.
#' @param total Logical; should total yield be included in plot?
#' @param log Logical; should the y-axis be log-transformed?
#' @param highlight Name of species to highlight in plot.
#' @param return_data Logical; should the underlying data be returned?
#' @param ... Other arguments passed to \code{\link[mizer]{getYield}}.
#' @return If \code{return_data = TRUE}, a data frame of the underlying
#'   data is returned.
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
        plot_dat <- reshape2::melt(y, varnames = c("Year", "Species"),
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
        ym$Simulation <- rep(1, nrow(ym))
        ym2$Simulation <- rep(2, nrow(ym2))
        ym <- rbind(ym, ym2)

        if (return_data) return(ym)

        plotDataFrame(ym, params,
                      ylab = "Yield [g/year]",
                      ytrans = ifelse(log, "log10", "identity"),
                      highlight = highlight, wrap_var = "Simulation")
    }
}
