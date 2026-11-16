#' Stored capabilities for shelf models
#'
#' `registerShelfCapabilities()` explicitly adopts the known shelf equations,
#' measurements, tuning strategy and transformations. Saved models are migrated
#' automatically; changing a provider afterwards requires a new declaration.
#' These callbacks consume the complete effective rates supplied by mizer.
#'
#' @param params,before MizerParams objects, before and after core bookkeeping.
#' @param n Consumer abundances.
#' @param n_pp Detritus abundance.
#' @param n_other Component states, including carrion biomass.
#' @param rates The complete effective rate list.
#' @param component Component identifier.
#' @param tol Relative convergence tolerance.
#' @param atol Absolute convergence tolerance.
#' @param operation Transformation identifier.
#' @param details Core species and grid maps, or a scaling factor.
#' @param targets Captured equation-rate targets.
#' @param ... Additional callback context.
#' @return The updated model, a definition, or the named quantities requested
#'   by the callback contract.
#' @name shelf_capabilities
NULL

shelf_fluxes <- function(params, n, n_pp, n_other, rates) {
    biomass <- n_other$carrion
    list(
        carrion_source = sum(getCarrionProduction(params, n, rates)),
        carrion_loss = unname(
            carrion_consumption_ms(params, n, rates) * biomass
        ),
        detritus_source = sum(getDetritusProduction(params, n, n_other, rates)),
        detritus_loss = detritus_consumption(params, n_pp, rates)
    )
}

#' @rdname shelf_capabilities
#' @export
carrion_equation <- function(params, n, n_pp, n_other, rates, ...) {
    flux <- shelf_fluxes(params, n, n_pp, n_other, rates)
    n_other$carrion * 0 + flux$carrion_source - flux$carrion_loss
}

#' @rdname shelf_capabilities
#' @export
detritus_equation <- function(params, n, n_pp, n_other, rates, ...) {
    biomass <- detritus_biomass(params, n_pp)
    if (!is.finite(biomass) || biomass <= 0) {
        stop("Detritus equations require a positive biomass and stored shape.")
    }
    flux <- shelf_fluxes(params, n, n_pp, n_other, rates)
    n_pp * ((flux$detritus_source - flux$detritus_loss) / biomass)
}

#' @rdname shelf_capabilities
#' @export
carrion_assessment <- function(params, n, n_pp, n_other, rates,
                               tol, atol, ...) {
    data.frame(measure = "biomass_rate",
               value = carrion_equation(params, n, n_pp, n_other, rates),
               tolerance = atol + tol * abs(n_other$carrion), units = "g/year")
}

#' @rdname shelf_capabilities
#' @export
detritus_assessment <- function(params, n, n_pp, n_other, rates,
                                tol, atol, ...) {
    flux <- shelf_fluxes(params, n, n_pp, n_other, rates)
    data.frame(measure = "biomass_rate",
               value = flux$detritus_source - flux$detritus_loss,
               tolerance = atol + tol * abs(detritus_biomass(params, n_pp)),
               units = "g/year")
}

#' @rdname shelf_capabilities
#' @export
carrion_measurement <- function(params, n_other, component, ...) {
    data.frame(series = "biomass", value = n_other[[component]],
               label = "Carrion", source = paste0("component:", component))
}

#' @rdname shelf_capabilities
#' @export
shelf_consumer_rates <- function(params, rates, ...) {
    list(growth = rates$e_growth, diffusion = rates$diffusion,
         mortality = rates$mort, recruitment = rates$rdd)
}

#' @rdname shelf_capabilities
#' @export
shelf_equation_rates <- function(params, n, n_pp, n_other, rates, ...) {
    flux <- shelf_fluxes(params, n, n_pp, n_other, rates)
    list(resource = list(source = flux$detritus_source,
                         loss = flux$detritus_loss),
         `component:carrion` = list(source = flux$carrion_source,
                                    loss = flux$carrion_loss))
}

#' @rdname shelf_capabilities
#' @export
carrion_equation_rates <- function(params, n, n_pp, n_other, rates, ...) {
    shelf_equation_rates(params, n, n_pp, n_other, rates)[["component:carrion"]]
}

#' @rdname shelf_capabilities
#' @export
detritus_equation_rates <- function(params, n, n_pp, n_other, rates, ...) {
    shelf_equation_rates(params, n, n_pp, n_other, rates)$resource
}

#' @rdname shelf_capabilities
#' @export
shelf_tuning_targets <- function(params, n, n_pp, n_other, rates, ...) {
    value <- shelf_equation_rates(params, n, n_pp, n_other, rates)
    value[["component:carrion"]]$loss <- value[["component:carrion"]]$source
    value$resource$source <- value$resource$loss
    value
}

#' @rdname shelf_capabilities
#' @export
shelf_balance <- function(params, n, n_pp, n_other, rates, targets, ...) {
    biomass <- n_other$carrion
    if (!is.numeric(biomass) || length(biomass) != 1L ||
            !is.finite(biomass) || biomass <= 0) {
        stop("Carrion balancing requires a positive biomass.")
    }
    # Both adjustable parameters are removed algebraically, using the same
    # effective context as target capture rather than new selective getters.
    fish_loss <- carrion_consumption_ms(params, n, rates) -
        params$other_params$carrion$decompose
    decompose <- targets[["component:carrion"]]$loss / biomass - fish_loss
    if (!is.finite(decompose) || decompose < 0) {
        stop("There is not enough carrion production.")
    }
    params$other_params$carrion$decompose <- decompose
    production <- getDetritusProduction(params, n, n_other, rates)
    external <- targets$resource$source -
        sum(production[names(production) != "external"])
    if (!is.finite(external)) stop("Detritus balancing requires finite inflow.")
    params$other_params$detritus$external <- external
    params
}

shelf_transformation_map <- function() {
    operations <- c("add_species", "remove_species", "reorder_species",
                    "rename_species", "size_grid", "scale_abundance",
                    "scale_rates")
    definitions <- lapply(operations, function(x) list(action = "none"))
    names(definitions) <- operations
    definitions
}

#' @rdname shelf_capabilities
#' @export
shelf_transform <- function(params, before, operation, details, ...) {
    if (operation %in% c("scale_abundance", "scale_rates")) {
        factor <- details$factor
        multiplier <- if (operation == "scale_abundance") 1 / factor else factor
        params$other_params$carrion$rho <-
            before$other_params$carrion$rho * multiplier
        params$species_params$rho_carrion <-
            before$species_params$rho_carrion * multiplier
        params$given_species_params$rho_carrion <-
            params$species_params$rho_carrion
        params$other_params$detritus$external <-
            before$other_params$detritus$external * factor
        if (operation == "scale_abundance") {
            params$initial_n_other$carrion <-
                before$initial_n_other$carrion * factor
        } else {
            params$other_params$carrion$decompose <-
                before$other_params$carrion$decompose * factor
            params$species_params$gear_mort <-
                before$species_params$gear_mort * factor
            params$given_species_params$gear_mort <-
                params$species_params$gear_mort
        }
        return(params)
    }
    for (field in c("gear_mort", "discard")) {
        value <- params$species_params[[field]]
        if (is.null(value)) value <- rep(NA_real_, nrow(params$initial_n))
        if (anyNA(value)) {
            value[is.na(value)] <- 0
            params$species_params[[field]] <- value
            signal_info(field, paste0("Missing ", field,
                                      " defaults to 0 for added species."),
                        level = 1, unhandled = "show")
        }
    }
    coefficients <- params$species_params$rho_carrion
    if (is.null(coefficients)) {
        coefficients <- rep(NA_real_, nrow(params$initial_n))
    }
    missing <- is.na(coefficients)
    if (any(missing)) {
        coefficients[missing] <- 0
        signal_info("rho_carrion", paste0(
            "Missing rho_carrion defaults to 0 for added species. Supply ",
            "rho_carrion explicitly to give them carrion encounter."
        ), level = 1, unhandled = "show")
    }
    if (any(!is.finite(coefficients)) || any(coefficients < 0)) {
        stop("rho_carrion must contain finite nonnegative coefficients.")
    }
    params$species_params$rho_carrion <- coefficients
    rho <- outer(coefficients, params$w ^ params$resource_params$n)
    dimnames(rho) <- dimnames(params$initial_n)
    old_species <- which(!is.na(details$species_map))
    old_bins <- which(!is.na(details$w_map))
    rho[details$species_map[old_species], details$w_map[old_bins]] <-
        before$other_params$carrion$rho[old_species, old_bins, drop = FALSE]
    params$other_params$carrion$rho <- rho
    params
}

# Every relevant method must be the core method. Checking only stored names
# misses an extension or locally registered S3 override of the effective route.
shelf_known_methods <- function(params) {
    generics <- c(
        "project", "project_simple", "projectRates", "projectEncounter",
        "projectFeedingLevel", "projectEReproAndGrowth", "projectERepro",
        "projectEGrowth", "projectDiffusion", "projectPredRate",
        "projectPredMort", "projectFMort", "projectMort", "projectRDI",
        "projectRDD", "projectResourceMort", "balanceRDD", "setParams",
        "setInteraction", "setPredKernel", "setMaxIntakeRate",
        "setMetabolicRate", "setExtMort", "setExtEncounter", "setExtDiffusion",
        "setSearchVolume", "setReproduction", "setFishing", "setResource",
        "setColours", "setLinetypes"
    )
    all(vapply(generics, function(generic) {
        all(vapply(class(params), function(class_name) {
            method <- utils::getS3method(generic, class_name, optional = TRUE)
            if (is.null(method)) return(TRUE)
            expected <- get0(paste0(generic, ".", class_name),
                             envir = asNamespace("mizer"), inherits = FALSE)
            !is.null(expected) && identical(method, expected)
        }, logical(1)))
    }, logical(1)))
}

shelf_known_rates <- function(params) {
    expected <- getRateFunction(params)
    expected[names(expected) != "RDD"] <- lapply(
        names(expected)[names(expected) != "RDD"],
        function(x) paste0("mizer", x)
    )
    expected$Mort <- params$rates_funcs$Mort
    mortality <- c("mizerMort", "seMort", "mizerShelf::seMort")
    reproduction <- c("BevertonHoltRDD", "RickerRDD", "SheperdRDD", "noRDD",
                      "constantRDD")
    shelf_known_methods(params) &&
        identical(params$rates_funcs, expected) &&
        params$rates_funcs$Mort %in% mortality &&
        params$rates_funcs$RDD %in% reproduction &&
        all(class(params) %in% c("mizerShelf", "MizerParams")) &&
        setequal(names(params$other_encounter), "carrion") &&
        !length(params$other_mort)
}

#' @rdname shelf_capabilities
#' @export
registerShelfCapabilities <- function(params) {
    if (!identical(params$resource_dynamics, "detritus_dynamics") ||
            !identical(params$other_dynamics$carrion, "carrion_dynamics") ||
            !identical(params$other_encounter$carrion,
                       "encounter_contribution")) {
        stop("Shelf capability adoption requires the standard detritus and ",
             "carrion providers. Declare support for modified providers ",
             "explicitly.")
    }
    known <- shelf_known_rates(params)
    temporal <- if (known) TRUE else NULL
    no_action <- shelf_transformation_map()
    params <- setComponent(
        params, "carrion", rate_fun = "mizerShelf::carrion_equation",
        integration_order = 1L,
        convergence_fun = "mizerShelf::carrion_assessment", role = "dynamic",
        biomass_fun = "mizerShelf::carrion_measurement",
        biomass_filters = character(),
        stationary = temporal, autonomous = temporal,
        equation_rates_fun = "mizerShelf::carrion_equation_rates",
        colour = "peru"
    )
    params <- setProcess(
        params, "resource", role = "dynamic", autonomous = temporal,
        rate_fun = "mizerShelf::detritus_equation",
        convergence_fun = "mizerShelf::detritus_assessment",
        stationary = temporal,
        equation_rates_fun = "mizerShelf::detritus_equation_rates",
        tuning = NULL
    )
    if (known) {
        consumer <- getProcess(params, "consumers")$definition
        consumer$stationary <- TRUE
        consumer$autonomous <- TRUE
        consumer$equation_rates_fun <- "mizerShelf::shelf_consumer_rates"
        consumer$tuning <- list(default = "unchanged", strategies = list(
            unchanged = list(action = "none", evolve = "consumers")
        ))
        consumer$transformations <- no_action
        consumer$mortality_fun <- if (params$rates_funcs$Mort == "mizerMort") {
            "mizer::mizerMortalityBreakdown"
        } else {
            "mizerShelf::shelf_mortality_breakdown"
        }
        params <- setProcess(params, "consumers", definition = consumer)
        params <- setProcess(params, "reproduction",
                             definition = mizerReproductionDefinition(params),
                             transformations = no_action)
    }
    params <- setTuningStrategy(
        params, "shelf_budgets", owners = c("resource", "component:carrion"),
        definition = list(
            action = "balance",
            hold = list(states = c("resource", "component:carrion"),
                        rates = character()),
            adjust = list(
                decomposition = list(path = c("other_params", "carrion",
                                              "decompose"), lower = 0),
                inflow = list(path = c("other_params", "detritus", "external"))
            ),
            balance_fun = "mizerShelf::shelf_balance",
            target_fun = "mizerShelf::shelf_tuning_targets"
        ), equation_rates_fun = "mizerShelf::shelf_equation_rates",
        stationary = known, default = TRUE
    )
    paths <- list(c("species_params", "rho_carrion"),
                  c("given_species_params", "rho_carrion"),
                  c("species_params", "gear_mort"),
                  c("given_species_params", "gear_mort"),
                  c("species_params", "discard"),
                  c("other_params", "detritus"))
    for (operation in if (known) names(no_action) else character()) {
        params <- setTransformation(
            params, paste0("shelf_", operation),
            owners = c("consumers", "resource", "component:carrion"),
            operation = operation,
            definition = list(action = "update",
                              transform_fun = "mizerShelf::shelf_transform",
                              paths = paths)
        )
    }
    params
}

#' Upgrade a shelf model's stored capabilities
#' @param object An earlier shelf model after the core upgrade.
#' @param ... Unused.
#' Existing preview declarations, including author removals, are retained.
#' Only older models acquire the initial standard declarations.
#' @return The upgraded model, preserving its effective mortality law.
#' @keywords internal
#' @exportS3Method utils::upgrade
upgrade.mizerShelf <- function(object, ...) {
    version <- utils::getFromNamespace("extensionVersion", "mizer")(
        object, "mizerShelf"
    )
    if (!is.na(version) && package_version(version) >= "1.2.0.9000") {
        return(object)
    }
    standard <- identical(object$resource_dynamics, "detritus_dynamics") &&
        identical(object$other_dynamics$carrion, "carrion_dynamics") &&
        identical(object$other_encounter$carrion, "encounter_contribution")
    if (standard) registerShelfCapabilities(object) else object
}
