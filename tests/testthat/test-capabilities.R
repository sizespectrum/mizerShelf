params <- validParams(NWMed_params)

# ---- steady-state diagnostics ----

# Mizer's steady-state diagnostics call the resource and component dynamics
# with only a subset of the rates, which used to make `getCarrionProduction()`
# fail and `isSteady()` silently report FALSE.

test_that("getSteadyResidual works on a shelf model", {
    residual <- getSteadyResidual(params)
    expect_equal(dim(residual), dim(params@initial_n))
    expect_true(any(is.finite(residual)))
})

test_that("the example model is recognised as being at steady state", {
    expect_true(isSteady(params))
})

test_that("a model moved off its steady state is recognised as such", {
    p <- params
    p@initial_n["Hake", ] <- p@initial_n["Hake", ] * 3
    expect_false(isSteady(p))
})

# ---- tuneSteadyState ----

test_that("tuneSteadyState returns a mizerShelf object at steady state", {
    p <- tuneSteadyState(params, t_max = 5, progress_bar = FALSE)
    expect_s3_class(p, "mizerShelf")
    expect_true(isSteady(p))
})

test_that("tuneSteadyState tunes carrion and detritus", {
    p <- params
    # Break the carrion and detritus balance
    p@other_params$carrion$decompose <- 0
    p@other_params$detritus$external <- 0
    p <- tuneSteadyState(p, t_max = 5, progress_bar = FALSE)
    expect_equal(p@other_params$carrion$decompose,
                 tune_carrion_detritus(p)@other_params$carrion$decompose)
    expect_equal(p@other_params$detritus$external,
                 tune_carrion_detritus(p)@other_params$detritus$external)
})

test_that("tuneSteadyState does not warn about the carrion component", {
    expect_no_warning(tuneSteadyState(params, t_max = 5, progress_bar = FALSE))
})

# ---- steady (superseded) ----

test_that("steady still works and tunes carrion and detritus", {
    p <- steady(params, t_max = 5, progress_bar = FALSE)
    expect_s3_class(p, "mizerShelf")
    expect_true(isSteady(p))
})

test_that("steady with return_sim returns a sim with tuned params", {
    sim <- steady(params, t_max = 5, return_sim = TRUE, progress_bar = FALSE)
    expect_s3_class(sim, "mizerShelfSim")
    expect_true(isSteady(sim@params))
})


test_that("biomass inclusion shares the core params and simulation contract", {
    p <- shelf_small
    default <- getBiomass(p)
    expect_length(default, nrow(p$species_params))
    include <- c("species", "resource", "component:carrion")
    measured <- getBiomass(p, include = include)
    expect_s3_class(measured, "MizerBiomass")
    expect_equal(measured[["component:carrion/biomass"]],
                 p$initial_n_other$carrion)
    expect_equal(measured[["resource"]], detritus_biomass(p))
    expect_true(all(attr(measured, "coverage")$available))
    simulation <- project(p, t_max = 0.02, dt = 0.01, t_save = 0.01,
                          progress_bar = FALSE, info_level = 0)
    series <- getBiomass(simulation, include = include)
    expect_s3_class(series, "ArrayTimeBySpecies")
    expect_equal(as.numeric(series[1, ]), as.numeric(measured))
    final <- getBiomass(finalParams(simulation), include = include)
    expect_equal(as.numeric(series[nrow(series), ]), as.numeric(final))
    expect_equal(attr(series, "series")$id, names(measured))
    filtered <- getBiomass(p, include = "component:carrion", min_w = 1)
    expect_true(is.na(filtered[[1]]))
    expect_match(attr(filtered, "unavailable")$reason, "filter")
})

test_that("shelf measurements and budgets use both quadrature schemes", {
    for (second in c(FALSE, TRUE)) {
        p <- shelf_small
        second_order_w(p) <- c(bin_average = second)
        rates <- getRates(p)
        expect_equal(getBiomass(p, include = "resource")[["resource"]],
                     detritus_biomass(p))
        expect_equal(sum(getDetritusConsumption(p)),
                     detritus_consumption(p, rates = rates))
        balanced <- tune_carrion_detritus(p)
        assessment <- getSteadyAssessment(balanced)
        measures <- assessment$measures
        expect_true(all(measures$passed[measures$owner != "consumers"]))
        expect_equal(getRates(balanced), rates, ignore_attr = TRUE)
    }
})

test_that("shelf transformations repair rho before dependent rates", {
    p <- shelf_small
    old_species <- p$species_params$species
    removed <- removeSpecies(p, old_species[2])
    expect_equal(removed$other_params$carrion$rho,
                 p$other_params$carrion$rho[-2, , drop = FALSE])
    expect_equal(dim(getEncounter(removed)), dim(removed$initial_n))
    reordered <- reorderSpecies(p, rev(old_species))
    expect_equal(reordered$other_params$carrion$rho,
                 p$other_params$carrion$rho[3:1, , drop = FALSE])
    renamed <- renameSpecies(p, setNames("New label", old_species[1]))
    expect_identical(rownames(renamed$other_params$carrion$rho)[1], "New label")
    expect_equal(unname(renamed$other_params$carrion$rho),
                 unname(p$other_params$carrion$rho))
    for (second in c(FALSE, TRUE)) {
        q <- p
        second_order_w(q) <- c(bin_average = second)
        resized <- adjustSizeGrid(q, no_w = 30)
        expect_equal(dim(resized$other_params$carrion$rho),
                     dim(resized$initial_n))
        expect_true(all(is.finite(getRates(resized)$encounter)))
    }
})

test_that("added species respect explicit rho and steady FALSE", {
    p <- shelf_small
    new <- p$species_params[1, , drop = FALSE]
    new$species <- "New species"
    new$rho_carrion <- 0.02
    local_mocked_bindings(
        steadySingleSpecies = function(...) stop("Unexpected steady solver"),
        .package = "mizer"
    )
    added <- addSpecies(p, new, steady = FALSE, info_level = 0)
    expect_equal(unname(added$species_params$rho_carrion[4]), 0.02)
    expect_equal(added$other_params$carrion$rho[4, ],
                 0.02 * added$w ^ added$resource_params$n, ignore_attr = TRUE)
    expect_equal(added$other_params$carrion$rho[1:3, ],
                 p$other_params$carrion$rho)
    new$rho_carrion <- NULL
    zero <- addSpecies(p, new, steady = FALSE, info_level = 0)
    expect_equal(unname(zero$species_params$rho_carrion[4]), 0)
    expect_true(all(zero$other_params$carrion$rho[4, ] == 0))
    expect_true(all(is.finite(getRates(zero)$mort)))
})

test_that("shelf scaling preserves units and effective consumer rates", {
    p <- shelf_small
    before <- getRates(p)
    scaled <- scaleModel(p, 3)
    expect_equal(scaled$initial_n_other$carrion, 3 * p$initial_n_other$carrion)
    expect_equal(scaled$other_params$carrion$rho,
                 p$other_params$carrion$rho / 3)
    expect_equal(getRates(scaled)$encounter, before$encounter)
    fast <- scaleRates(p, 2)
    expect_equal(fast$other_params$carrion$decompose,
                 2 * p$other_params$carrion$decompose)
    expect_equal(fast$species_params$gear_mort, 2 * p$species_params$gear_mort)
    expect_equal(getRates(fast)$encounter, 2 * before$encounter)
})

test_that("legacy adoption preserves the shipped mortality and state", {
    old <- NWMed_params
    old$capabilities <- NULL
    old$mizer_version <- package_version("3.4.0.9003")
    old <- recordExtension(old, "mizerShelf", version = "1.1.0")
    upgraded <- with_info_level(validParams(old), info_level = 0)
    expect_identical(upgraded$rates_funcs$Mort, "mizerMort")
    expect_equal(upgraded$initial_n, old$initial_n)
    expect_equal(upgraded$initial_n_pp, old$initial_n_pp)
    expect_equal(upgraded$initial_n_other, old$initial_n_other)
    expect_true("shelf_budgets" %in% names(upgraded$capabilities$tuning))
    expect_identical(getComponent(upgraded, "carrion")$role, "dynamic")
    expect_true(getMortByCause(upgraded)$complete)
    expect_false("gear" %in% names(getMortByCause(upgraded)$causes))
})

test_that("joint tuning captures a whole-model final assessment", {
    p <- params
    tuned <- tuneSteadyState(p, t_max = 5, progress_bar = FALSE, info_level = 0)
    report <- attr(tuned, "convergence")
    expect_identical(report$assessment$scope, "whole")
    expect_true(report$assessment$verdict)
    expect_contains(report$search_assessment$fixed, "component:carrion")
    expect_contains(report$search_assessment$fixed, "resource")
    expect_equal(tuned$initial_n_other, p$initial_n_other)
    expect_equal(tuned$initial_n_pp, p$initial_n_pp)
})

test_that("missing transformation support stops before changing the model", {
    p <- shelf_small
    p$capabilities$transformations$shelf_remove_species <- NULL
    original <- p
    expect_error(removeSpecies(p, p$species_params$species[1]),
                 "transformation support")
    expect_equal(p, original)
})

test_that("stored shelf capabilities survive save and read", {
    p <- shelf_small
    path <- tempfile(fileext = ".rds")
    withr::defer(unlink(path))
    saveParams(p, path)
    restored <- readParams(path)
    expect_equal(restored$capabilities, p$capabilities)
    expect_equal(getBiomass(restored, include = "component:carrion"),
                 getBiomass(p, include = "component:carrion"))
    expect_equal(getSteadyAssessment(restored)$measures,
                 getSteadyAssessment(p)$measures)
})


test_that("unknown S3 rate routes are not adopted as known equations", {
    p <- shelf_small
    expect_true(shelf_known_rates(p))
    methods <- get(".__S3MethodsTable__.", envir = asNamespace("mizer"))
    key <- "projectRDI.mizerShelf"
    previous <- get0(key, envir = methods, inherits = FALSE)
    withr::defer({
        if (is.null(previous)) {
            if (exists(key, envir = methods, inherits = FALSE)) {
                rm(list = key, envir = methods)
            }
        } else {
            assign(key, previous, envir = methods)
        }
    })
    registerS3method("projectRDI", "mizerShelf", function(params, t, ...) {
        NextMethod() * (1 + t)
    }, envir = asNamespace("mizer"))
    expect_false(shelf_known_rates(p))
    adopted <- registerShelfCapabilities(p)
    expect_error(tuneSteadyState(adopted, info_level = 0),
                 "stationary|current|provider|support")
    extra <- p$species_params[1, ]
    extra$species <- "Added"
    rownames(extra) <- "Added"
    expect_error(addSpecies(adopted, extra, steady = FALSE),
                 "support|declaration")
    expect_null(getComponent(adopted, "carrion")$definition$autonomous)
    expect_null(getProcess(adopted, "resource")$definition$autonomous)
})


test_that("infeasible carrion budgets fail without changing the model", {
    p <- shelf_small
    p$species_params$rho_carrion <- rep(1e6, nrow(p$initial_n))
    p <- setRho(p)
    before <- p
    expect_error(tune_carrion_detritus(p), "not enough carrion production")
    expect_equal(p, before)
})


test_that("re-adoption follows a changed known mortality provider", {
    p <- shelf_small
    expect_true("gear" %in% names(getMortByCause(p)$causes))
    p <- setRateFunction(p, "Mort", "mizerMort")
    p <- registerShelfCapabilities(p)
    report <- getMortByCause(p)
    expect_true(report$complete)
    expect_false("gear" %in% names(report$causes))
    p <- registerShelfCapabilities(setRateFunction(p, "Mort", "seMort"))
    expect_true("gear" %in% names(getMortByCause(p)$causes))
})


test_that("Shelf preserves unknown carrion mortality without support", {
    assign("review_carrion_mort", function(params, ...) {
        params$initial_n * 0 + 0.1
    }, envir = globalenv())
    on.exit(rm("review_carrion_mort", envir = globalenv()))
    p <- setComponent(shelf_small, "carrion", mort_fun = "review_carrion_mort")
    rates <- getRates(p)
    adopted <- registerShelfCapabilities(p)
    expect_identical(adopted$other_mort$carrion, "review_carrion_mort")
    expect_equal(getRates(adopted), rates, ignore_attr = TRUE)
    expect_null(getComponent(adopted, "carrion")$definition$autonomous)
    expect_error(tuneSteadyState(adopted, info_level = 0),
                 "support|stationary|strategy")
    expect_error(scaleModel(adopted, 2), "support|transformation")
})


test_that("preview upgrades retain authored definitions and removals", {
    p <- setComponent(shelf_small, "carrion", convergence_fun = NULL,
                      biomass_fun = NULL, autonomous = FALSE, tuning = NULL)
    p <- setProcess(p, "resource", convergence_fun = NULL, autonomous = FALSE)
    p$capabilities$tuning$shelf_budgets <- NULL
    p$capabilities$transformations$shelf_scale_abundance <- NULL
    old <- recordExtension(p, "mizerShelf", version = "1.2.0.9000")
    definitions <- old$capabilities
    upgraded <- validParams(old)
    expect_identical(upgraded$capabilities, definitions)
    expect_identical(upgraded$initial_n_other, old$initial_n_other)
    expect_identical(mizer:::extensionVersion(upgraded, "mizerShelf"),
                     "1.2.0.9001")
    path <- tempfile(fileext = ".rds")
    withr::defer(unlink(path))
    saveRDS(old, path)
    expect_identical(readParams(path)$capabilities, definitions)
    expect_identical(upgrade.mizerShelf(upgraded), upgraded)
})
