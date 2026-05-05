params <- validParams(NWMed_params)
rates <- getRates(params)

# ---- gearMort ----

test_that("gearMort returns array with same dimensions as f_mort", {
    f_mort <- getFMort(params)
    gm <- gearMort(params, f_mort)
    expect_equal(dim(gm), dim(f_mort))
})

test_that("gearMort returns non-negative values", {
    f_mort <- getFMort(params)
    gm <- gearMort(params, f_mort)
    expect_true(all(gm >= 0))
})

test_that("gearMort is zero when fishing mortality exceeds gear_mort", {
    f_mort <- getFMort(params)
    # If gear_mort is less than f_mort, excess should be zero
    params_high_f <- params
    params_high_f@species_params$gear_mort <- 0
    gm <- gearMort(params_high_f, f_mort)
    expect_true(all(gm == 0))
})

test_that("gearMort equals gear_mort - f_mort where positive", {
    f_mort <- getFMort(params)
    gm <- gearMort(params, f_mort)
    gear_mort_mat <- matrix(params@species_params$gear_mort,
                            nrow = nrow(f_mort), ncol = ncol(f_mort))
    diff <- gear_mort_mat - f_mort
    expected <- pmax(diff, 0)
    expect_equal(gm, expected)
})

# ---- seMort ----

test_that("seMort returns array with species x size dimensions", {
    sm <- seMort(params, params@initial_n, params@initial_n_pp,
                 params@initial_n_other, 0, rates$f_mort, rates$pred_mort)
    expect_equal(dim(sm), c(nrow(params@species_params), length(params@w)))
})

test_that("seMort equals mizerMort plus gearMort", {
    sm <- seMort(params, params@initial_n, params@initial_n_pp,
                 params@initial_n_other, 0, rates$f_mort, rates$pred_mort)
    mm <- mizerMort(params, params@initial_n, params@initial_n_pp,
                    params@initial_n_other, 0, rates$f_mort, rates$pred_mort)
    gm <- gearMort(params, rates$f_mort)
    expect_equal(sm, mm + gm)
})

test_that("seMort is non-negative", {
    sm <- seMort(params, params@initial_n, params@initial_n_pp,
                 params@initial_n_other, 0, rates$f_mort, rates$pred_mort)
    expect_true(all(sm >= 0))
})
