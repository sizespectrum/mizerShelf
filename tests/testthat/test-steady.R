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
    expect_s4_class(p, "mizerShelf")
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
    expect_s4_class(p, "mizerShelf")
    expect_true(isSteady(p))
})

test_that("steady with return_sim returns a sim with tuned params", {
    sim <- steady(params, t_max = 5, return_sim = TRUE, progress_bar = FALSE)
    expect_s4_class(sim, "mizerShelfSim")
    expect_true(isSteady(sim@params))
})

# ---- balance_detritus_dynamics ----

test_that("balance_detritus_dynamics keeps rate and capacity unchanged", {
    balance <- balance_detritus_dynamics(params)
    expect_equal(balance$resource_rate, params@rr_pp)
    expect_equal(balance$resource_capacity, params@cc_pp)
})

test_that("balance_detritus_dynamics passes requested values through", {
    rr <- params@rr_pp * 2
    cc <- params@cc_pp * 3
    balance <- balance_detritus_dynamics(params, resource_rate = rr,
                                         resource_capacity = cc)
    expect_equal(balance$resource_rate, rr)
    expect_equal(balance$resource_capacity, cc)
})
