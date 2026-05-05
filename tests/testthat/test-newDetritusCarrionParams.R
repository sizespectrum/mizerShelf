params <- validParams(NWMed_params)

# ---- newDetritusCarrionParams ----

test_that("newDetritusCarrionParams returns a MizerParams object", {
    sp <- species_params(params)[1:3, ]
    p <- newDetritusCarrionParams(sp)
    expect_s4_class(p, "MizerParams")
})

test_that("newDetritusCarrionParams creates carrion component", {
    sp <- species_params(params)[1:3, ]
    p <- newDetritusCarrionParams(sp)
    expect_false(is.null(getComponent(p, "carrion")))
})

test_that("newDetritusCarrionParams uses detritus_dynamics for resource", {
    sp <- species_params(params)[1:3, ]
    p <- newDetritusCarrionParams(sp)
    expect_equal(p@resource_dynamics, "detritus_dynamics")
})

test_that("newDetritusCarrionParams uses seMort for mortality", {
    sp <- species_params(params)[1:3, ]
    p <- newDetritusCarrionParams(sp)
    expect_equal(p@rates_funcs$Mort, "seMort")
})

test_that("newDetritusCarrionParams rho has correct dimensions", {
    sp <- species_params(params)[1:3, ]
    p <- newDetritusCarrionParams(sp)
    expect_equal(nrow(p@other_params$carrion$rho), 3)
    expect_equal(ncol(p@other_params$carrion$rho), length(p@w))
})

# ---- scaleModel ----

test_that("scaleModel returns MizerParams", {
    expect_s4_class(scaleModel(params, 2), "MizerParams")
})

test_that("scaleModel scales rho inversely", {
    p2 <- scaleModel(params, 2)
    expect_equal(p2@other_params$carrion$rho,
                 params@other_params$carrion$rho / 2)
})

test_that("scaleModel scales rho_carrion species parameter inversely", {
    p2 <- scaleModel(params, 2)
    expect_equal(p2@species_params$rho_carrion,
                 params@species_params$rho_carrion / 2)
})

test_that("scaleModel scales external detritus supply proportionally", {
    p2 <- scaleModel(params, 3)
    expect_equal(p2@other_params$detritus$external,
                 params@other_params$detritus$external * 3)
})
