#' Remove some species from the model
#' 
#' This calls `mizer::removeSpecies()` and in addition removes the relevant
#' row from the carrion consumption array `rho`.
#' @param params A MizerParams object
#' @param species The species to be removed. A vector of species names, or a
#'   numeric vector of species indices, or a logical vector indicating for each
#'   species whether it is to be removed (TRUE) or not.
#' @return A MizerParams object with fewer species.
#' @examples
#' params <- NWMed_params
#' species_params(params)$species
#' params <- removeSpecies(params, c("Poor cod", "Horse mackerel"))
#' species_params(params)$species
#' @export
removeSpecies <- function(params, species) {
    p <- mizer::removeSpecies(params, species)
    species <- valid_species_arg(params, species,
                                 return.logical = TRUE)
    keep <- !species
    p@other_params$carrion$rho <-
        p@other_params$carrion$rho[keep, , drop = FALSE]
    p
}

#' Add new species
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Takes a \linkS4class{MizerParams} object and adds additional species with
#'   given parameters to the ecosystem. It sets the initial values for these new
#'   species to their steady-state solution in the given initial state of the
#'   existing ecosystem. This will be close to the true steady state if the
#'   abundances of the new species are sufficiently low. Hence the abundances of
#'   the new species are set so that they are at most 1/100th of the resource 
#'   power law. Their reproductive efficiencies are set so as to keep them at
#'   that low level.
#'
#' @param params A mizer params object for the original system.
#' @param species_params Data frame with the species parameters of the new
#'   species we want to add to the system.
#' @param interaction Interaction matrix. A square matrix giving either the
#'   interaction coefficients between all species or only those between the new
#'   species. In the latter case all interaction between an old and a new
#'   species are set to 1. If this argument is missing, all interactions
#'   involving a new species are set to 1.
#' @param gear_params Data frame with the gear parameters for the new
#'   species. If not provided then the new species will not be fished.
#' @param initial_effort A named vector with the effort for new fishing gear
#'   introduced in `gear_params`. New gear for which no effort is set via this
#'   vector will have an initial effort of 0. Should not include effort values
#'   for existing gear.
#'
#' @return An object of type \linkS4class{MizerParams}
#'
#' @details The resulting MizerParams object will use the same size grid where
#'   possible, but if one of the new species needs a larger range of w (either
#'   because a new species has an egg size smaller than those of existing
#'   species or a maximum size larger than those of existing species) then the
#'   grid will be expanded and all arrays will be enlarged accordingly.
#'
#'   If any of the rate arrays of the existing species had been set by the user
#'   to values other than those calculated as default from the species
#'   parameters, then these will be preserved. Only the rates for the new
#'   species will be calculated from their species parameters.
#'
#'   After adding the new species, the background species are not retuned and
#'   the system is not run to steady state. This could be done with [steady()].
#'   The new species will have a reproduction level of 1/4, this can then be
#'   changed with [setBevertonHolt()]
#'
#' @seealso [removeSpecies()]
#' @export
#' @examples
#' params <- newTraitParams()
#' species_params <- data.frame(
#'     species = "Mullet",
#'     w_max = 173,
#'     w_mat = 15,
#'     beta = 283,
#'     sigma = 1.8,
#'     h = 30,
#'     a = 0.0085,
#'     b = 3.11
#' )
#' params <- addSpecies(params, species_params)
#' plotSpectra(params)
addSpecies <- function(params, species_params,
                       gear_params = data.frame(), initial_effort,
                       interaction) {
  # check validity of parameters ----
  params <- validParams(params)
  species_params <- validSpeciesParams(species_params)
  gear_params <- validGearParams(gear_params, species_params)
  if (any(species_params$species %in% params@species_params$species)) {
    stop("You can not add species that are already there.")
  }
  if (!is.null(comment(params@pred_kernel))) {
    stop("addSpecies() can not add species to a MizerParams object that ",
         "has its predation kernel protected by a comment.")
  }
  if (!is.null(comment(params@selectivity))) {
    stop("addSpecies() can not add species to a MizerParams object that ",
         "has its selectivity array protected by a comment.")
  }
  if (!is.null(comment(params@catchability))) {
    stop("addSpecies() can not add species to a MizerParams object that ",
         "has its catchability array protected by a comment.")
  }
  
  # set interaction
  no_old_sp <- nrow(params@species_params)
  old_sp <- 1:no_old_sp
  no_new_sp <- nrow(species_params)
  new_sp <- 1:no_new_sp + no_old_sp
  no_sp <- no_old_sp + no_new_sp
  if (missing(interaction)) {
    # keep existing interactions between old species and
    # set interactions involving new species to 1
    inter <- matrix(1, nrow = no_sp, ncol = no_sp)
    inter[old_sp, old_sp] <- params@interaction
  } else if (all(dim(interaction) == c(no_new_sp, no_new_sp))) {
    # keep existing interactions between old species,
    # set interactions involving an old and a new species to 1
    # and use supplied matrix for interaction among new species
    inter <- matrix(1, nrow = no_sp, ncol = no_sp)
    inter[old_sp, old_sp] <- params@interaction
    inter[new_sp, new_sp] <- interaction
  } else if (all(dim(interaction) != c(no_sp, no_sp))) {
    stop("Interaction matrix has invalid dimensions.")
  } else {
    inter <- interaction
  }
  
  # combine species params ----
  
  # Move linecolour and linetype into species_params
  params@species_params$linetype <-
    params@linetype[as.character(params@species_params$species)]
  params@species_params$linecolour <-
    params@linecolour[as.character(params@species_params$species)]
  
  # Make sure that all columns exist in both data frames
  missing <- setdiff(names(params@species_params), names(species_params))
  species_params[missing] <- NA
  missing <- setdiff(names(species_params), names(params@species_params))
  params@species_params[missing] <- NA
  
  # add the new species (with parameters described by species_params),
  # to make a larger species_params dataframe.
  combi_species_params <- rbind(params@species_params, species_params,
                                stringsAsFactors = FALSE)
  
  # combine gear params ----
  if (!all(gear_params$species %in% species_params$species)) {
    stop("gear_params should only set gear parameters for new species.")
  }
  # Make sure that all columns exist in both data frames
  if (nrow(gear_params) > 0) {
    missing <- setdiff(names(params@gear_params), names(gear_params))
    gear_params[missing] <- NA
  }
  if (nrow(params@gear_params) > 0) {
    missing <- setdiff(names(gear_params), names(params@gear_params))
    params@gear_params[missing] <- NA
  }
  combi_gear_params <- rbind(params@gear_params, gear_params,
                             stringsAsFactors = FALSE)
  
  # expand grid ----
  # in case the new species need a bigger range of w
  # We need to make sure that the new grid that newMultispeciesParams()
  # will create contains the old grid as a subgrid.
  no_w <- length(params@w)
  no_w_full <- length(params@w_full)
  max_w <- max(params@w)
  min_w <- min(params@w)
  new_max_w <- max_w
  new_min_w <- min_w
  new_no_w <- no_w
  extra_no_w <- 0  # extra bins added for smaller egg size
  if (max(species_params$w_max) > max(params@w) + .Machine$double.eps) {
    new_max_w <- max(species_params$w_max)
    dx <- log10(max_w / min_w) / (no_w - 1)
    new_no_w <- ceiling(log10(new_max_w / min_w) / dx) + 1
    new_max_w <- min_w * 10^(dx * (new_no_w - 1))
  }
  if (min(species_params$w_min) < min(params@w) - .Machine$double.eps) {
    new_min_w <- min(species_params$w_min)
    if (new_min_w < min(params@w_full)) {
      stop("The smallest egg size is too small.")
    }
    # We need to set the smallest egg size to a size on the existing grid
    # so that the new grid will be compatible
    sel_min <- combi_species_params$w_min == new_min_w
    new_min_w <- max(params@w_full[params@w_full <= new_min_w])
    combi_species_params$w_min[sel_min] <- new_min_w
    
    extra_no_w <- sum(params@w_full >= new_min_w) - no_w
    new_no_w <- new_no_w + extra_no_w
  }
  
  # new params object ----
  # use dataframe and global settings from params to make a new MizerParams
  # object.
  p <- newDetritusCarrionParams(
    combi_species_params,
    interaction = inter,
    max_w = new_max_w,
    # for min_w_pp we choose something that will then be rounded down
    # to the existing smallest size when emptyParams() creates the new grid
    w_min_detritus = (params@w_full[[1]] + params@w_full[[2]]) / 2,
    no_w = new_no_w,
    gear_params = combi_gear_params,
    kappa = params@resource_params$kappa,
    n = params@resource_params[["n"]],
    lambda = params@resource_params$lambda,
    w_max_detritus = params@resource_params$w_pp_cutoff
  )
  
  # Set effort ----
  new_gear <- setdiff(unique(gear_params$gear),
                      unique(params@gear_params$gear))
  p@initial_effort[names(params@initial_effort)] <- params@initial_effort
  if (!missing(initial_effort)) {
    if (is.null(names(initial_effort))) {
      stop("The `initial_effort` must be a named list or vector.")
    }
    if (!all(names(initial_effort) %in% new_gear)) {
      stop("The names of the `initial_effort` do not match the names of the new gears.")
    }
    p@initial_effort[names(initial_effort)] <- initial_effort
  }
  
  # Keep resource spectrum ----
  p@initial_n_pp[1:no_w_full] <- params@initial_n_pp
  p@cc_pp[1:no_w_full] <- params@cc_pp
  p@rr_pp[1:no_w_full] <- params@rr_pp
  p@resource_dynamics <- params@resource_dynamics
  p@resource_params <- params@resource_params
  
  # Preserve comments ----
  comment(p) <- comment(params)
  for (slot in (slotNames(p))) {
    comment(slot(p, slot)) <- comment(slot(params, slot))
  }
  
  # Copy old data ----
  # selector for old w bins inside new w
  old_w <- (extra_no_w + 1):(extra_no_w + no_w)
  p@A[old_sp] <- params@A
  p@psi[old_sp, old_w] <- params@psi
  p@maturity[old_sp, old_w] <- params@maturity
  p@sc[old_w] <- params@sc
  p@mu_b[old_sp, old_w] <- params@mu_b
  p@intake_max[old_sp, old_w] <- params@intake_max
  p@search_vol[old_sp, old_w] <- params@search_vol
  p@metab[old_sp, old_w] <- params@metab
  
  p@other_dynamics <- params@other_dynamics
  p@other_encounter <- params@other_encounter
  p@other_mort <- params@other_mort
  p@rates_funcs <- params@rates_funcs
  
  rho <- p@other_params$carrion$rho
  p@other_params <- params@other_params
  p@other_params$carrion$rho <- rho
  
  p@metadata <- params@metadata
  p@time_created <- params@time_created
  p@mizer_version <- params@mizer_version
  p@extensions <- params@extensions
  
  # The following does not affect the new species but preserves
  # any changes the user might have made in the original params object
  p <- setColours(p, params@linecolour)
  p <- setLinetypes(p, params@linetype)
  
  # we assume same background death for all species
  # p@mu_b[new_sp, ] <- rep(params@mu_b[1, ], each = no_new_sp)
  
  # initial solution ----
  p@initial_n[old_sp, old_w] <- params@initial_n
  # Turn off self-interaction among the new species, so we can determine the
  # growth rates, and death rates induced upon them by the pre-existing species
  p@interaction[new_sp, new_sp] <- 0
  mumu <- getMort(p)
  gg <- getEGrowth(p)
  
  # Compute solution for new species
  for (i in new_sp) {
    g <- gg[i, ]
    mu <- mumu[i, ]
    w_max_idx <- sum(p@w < p@species_params$w_max[i])
    idx <- p@w_min_idx[i]:(w_max_idx - 1)
    if (any(g[idx] == 0)) {
      stop("Can not compute steady state due to zero growth rates for ",
           p@species_params$species[i])
    }
    p@initial_n[i, ] <- 0
    p@initial_n[i, p@w_min_idx[i]:w_max_idx] <-
      c(1, cumprod(g[idx] / ((g + mu * p@dw)[idx + 1])))
    
    # set low abundance ----
    # Normalise solution so that it is never more than 1/100th of the
    # Sheldon spectrum.
    # We look at the maximum of abundance times w^lambda
    # because that is always an increasing function at small size.
    idx <- which.max(p@initial_n[i, ] * p@w^p@resource_params$lambda)
    p@initial_n[i, ] <- p@initial_n[i, ] *
      p@resource_params$kappa * p@w[idx]^(-p@resource_params$lambda) / 
      p@initial_n[i, idx] / 100
    p@A[i] <- sum(p@initial_n[i, ] * p@w * p@dw * p@maturity[i, ])
  }
  
  if (any(is.infinite(p@initial_n))) {
    stop("Candidate steady state holds infinities.")
  }
  if (any(is.na(p@initial_n) | is.nan(p@initial_n))) {
    stop("Candidate steady state holds non-numeric values.")
  }
  
  # Turn self interaction back on
  p@interaction[new_sp, new_sp] <- inter[new_sp, new_sp]
  
  # Retune reproductive efficiencies of new species
  repro_level <- rep(1 / 4, length(new_sp))
  names(repro_level) <- p@species_params$species[new_sp]
  p <- setBevertonHolt(p, reproduction_level = repro_level)
  
  return(p)
}