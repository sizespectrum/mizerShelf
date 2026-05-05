params <- validParams(NWMed_params)

# ---- encounter_contribution ----

test_that("encounter_contribution returns array (species x size)", {
    ec <- encounter_contribution(params, params@initial_n_other, "carrion")
    expect_equal(dim(ec), dim(params@other_params$carrion$rho))
})

test_that("encounter_contribution is proportional to biomass", {
    ec1 <- encounter_contribution(params, params@initial_n_other, "carrion")
    n_other2 <- params@initial_n_other
    n_other2$carrion <- n_other2$carrion * 2
    ec2 <- encounter_contribution(params, n_other2, "carrion")
    expect_equal(ec2, 2 * ec1)
})

# ---- carrion_biomass ----

test_that("carrion_biomass returns a positive scalar", {
    b <- carrion_biomass(params)
    expect_length(b, 1)
    expect_gt(b, 0)
})

test_that("carrion_biomass reads from initial_n_other$carrion", {
    expect_equal(carrion_biomass(params), params@initial_n_other$carrion)
})

# ---- detritus_biomass ----

test_that("detritus_biomass returns a positive scalar", {
    b <- detritus_biomass(params)
    expect_length(b, 1)
    expect_gt(b, 0)
})

test_that("detritus_biomass scales linearly with n_pp", {
    b1 <- detritus_biomass(params)
    b2 <- detritus_biomass(params, n_pp = params@initial_n_pp * 3)
    expect_equal(b2, 3 * b1)
})

# ---- carrion_consumption_ms ----

test_that("getCarrionConsumption and carrion_consumption_ms agree", {
    expect_equal(sum(getCarrionConsumption(params)),
                 carrion_consumption_ms(params) * carrion_biomass(params))
})

test_that("carrion_consumption_ms returns a positive scalar", {
    cms <- carrion_consumption_ms(params)
    expect_length(cms, 1)
    expect_gt(cms, 0)
})

# ---- getCarrionConsumption ----

test_that("getCarrionConsumption returns named vector with all species + decompose", {
    cc <- getCarrionConsumption(params)
    expect_named(cc, c(as.character(params@species_params$species), "decompose"))
})

test_that("getCarrionConsumption values are non-negative", {
    cc <- getCarrionConsumption(params)
    expect_true(all(cc >= 0))
})

# ---- getCarrionProduction ----

test_that("getCarrionProduction returns named vector with expected entries", {
    cp <- getCarrionProduction(params)
    expect_named(cp, c("ext_mort", "gear_mort", "discards"))
})

test_that("getCarrionProduction values are non-negative", {
    cp <- getCarrionProduction(params)
    expect_true(all(cp >= 0))
})

# ---- carrion_dynamics ----

test_that("carrion_dynamics returns a positive scalar", {
    rates <- getRates(params)
    cd <- carrion_dynamics(params, params@initial_n, params@initial_n_other, rates, dt = 0.1)
    expect_length(cd, 1)
    expect_gt(cd, 0)
})

test_that("carrion_dynamics is approximately constant at steady state", {
    rates <- getRates(params)
    cd <- carrion_dynamics(params, params@initial_n, params@initial_n_other, rates, dt = 0.1)
    expect_equal(cd, carrion_biomass(params), tolerance = 1e-6)
})

# ---- detritus_consumption ----

test_that("getDetritusConsumption and detritus_consumption agree", {
    expect_equal(sum(getDetritusConsumption(params)),
                 detritus_consumption(params))
})

test_that("detritus_consumption returns a positive scalar", {
    dc <- detritus_consumption(params)
    expect_length(dc, 1)
    expect_gt(dc, 0)
})

# ---- getDetritusConsumption ----

test_that("getDetritusConsumption returns named vector with one entry per species", {
    dc <- getDetritusConsumption(params)
    expect_length(dc, nrow(params@species_params))
    expect_named(dc, as.character(params@species_params$species))
})

test_that("getDetritusConsumption values are non-negative", {
    dc <- getDetritusConsumption(params)
    expect_true(all(dc >= 0))
})

# ---- getDetritusProduction ----

test_that("getDetritusProduction returns named vector with feces, carrion, external", {
    dp <- getDetritusProduction(params)
    expect_named(dp, c("feces", "carrion", "external"))
})

test_that("getDetritusProduction values are non-negative", {
    dp <- getDetritusProduction(params)
    expect_true(all(dp >= 0))
})

# ---- detritus_dynamics ----

test_that("detritus_dynamics returns vector of same length as n_pp", {
    rates <- getRates(params)
    dd <- detritus_dynamics(params, params@initial_n, params@initial_n_pp,
                            params@initial_n_other, rates, dt = 0.1)
    expect_length(dd, length(params@initial_n_pp))
})

test_that("detritus_dynamics is approximately constant at steady state", {
    rates <- getRates(params)
    dd <- detritus_dynamics(params, params@initial_n, params@initial_n_pp,
                            params@initial_n_other, rates, dt = 0.1)
    expect_equal(detritus_biomass(params, dd), detritus_biomass(params), tolerance = 1e-6)
})

# ---- carrion_lifetime ----

test_that("carrion_lifetime is inverse of carrion_consumption_ms", {
    expect_equal(carrion_lifetime(params), 1 / carrion_consumption_ms(params))
})

test_that("carrion_lifetime<- doubles the lifetime when set to 2x", {
    lt <- carrion_lifetime(params)
    p2 <- params
    carrion_lifetime(p2) <- 2 * lt
    expect_equal(carrion_lifetime(p2), 2 * lt, tolerance = 1e-10)
})

test_that("carrion_lifetime<- preserves total consumption", {
    lt <- carrion_lifetime(params)
    p2 <- params
    carrion_lifetime(p2) <- 2 * lt
    expect_equal(sum(getCarrionConsumption(p2)), sum(getCarrionConsumption(params)),
                 tolerance = 1e-10)
})

# ---- detritus_lifetime ----

test_that("detritus_lifetime equals biomass / consumption", {
    expect_equal(detritus_lifetime(params),
                 detritus_biomass(params) / detritus_consumption(params))
})

test_that("detritus_lifetime<- doubles the lifetime when set to 2x", {
    dlt <- detritus_lifetime(params)
    p2 <- params
    detritus_lifetime(p2) <- 2 * dlt
    expect_equal(detritus_lifetime(p2), 2 * dlt, tolerance = 1e-10)
})

test_that("detritus_lifetime<- preserves total detritus consumption", {
    dlt <- detritus_lifetime(params)
    p2 <- params
    detritus_lifetime(p2) <- 2 * dlt
    expect_equal(detritus_consumption(p2), detritus_consumption(params),
                 tolerance = 1e-10)
})

# ---- carrion_human_origin ----

test_that("carrion_human_origin returns value between 0 and 1", {
    cho <- carrion_human_origin(params)
    expect_gt(cho, 0)
    expect_lt(cho, 1)
})

test_that("carrion_human_origin equals (gear_mort + discards) / total production", {
    cp <- getCarrionProduction(params)
    expected <- (cp[["gear_mort"]] + cp[["discards"]]) / sum(cp)
    expect_equal(carrion_human_origin(params), expected)
})

# ---- rescale_carrion ----

test_that("rescale_carrion multiplies carrion biomass by factor", {
    p2 <- rescale_carrion(params, 3)
    expect_equal(carrion_biomass(p2), 3 * carrion_biomass(params))
})

test_that("rescale_carrion preserves total carrion consumption", {
    p2 <- rescale_carrion(params, 3)
    expect_equal(sum(getCarrionConsumption(p2)), sum(getCarrionConsumption(params)),
                 tolerance = 1e-10)
})

test_that("rescale_carrion adjusts rho inversely to factor", {
    p2 <- rescale_carrion(params, 2)
    expect_equal(p2@other_params$carrion$rho, params@other_params$carrion$rho / 2)
})

# ---- rescale_detritus ----

test_that("rescale_detritus multiplies detritus biomass by factor", {
    p2 <- rescale_detritus(params, 4)
    expect_equal(detritus_biomass(p2), 4 * detritus_biomass(params))
})

test_that("rescale_detritus preserves total detritus consumption", {
    p2 <- rescale_detritus(params, 4)
    expect_equal(detritus_consumption(p2), detritus_consumption(params),
                 tolerance = 1e-10)
})

# ---- rescaleComponents ----

test_that("rescaleComponents applies both carrion and detritus scaling", {
    p2 <- rescaleComponents(params, carrion_factor = 2, detritus_factor = 3)
    expect_equal(carrion_biomass(p2), 2 * carrion_biomass(params))
    expect_equal(detritus_biomass(p2), 3 * detritus_biomass(params))
})

test_that("rescaleComponents with default factors leaves params unchanged", {
    p2 <- rescaleComponents(params)
    expect_equal(carrion_biomass(p2), carrion_biomass(params))
    expect_equal(detritus_biomass(p2), detritus_biomass(params))
})

# ---- tune_carrion_detritus ----

test_that("tune_carrion_detritus returns MizerParams", {
    p2 <- tune_carrion_detritus(params)
    expect_s4_class(p2, "MizerParams")
})

test_that("tune_carrion_detritus sets carrion to steady state", {
    p2 <- tune_carrion_detritus(params)
    cp <- sum(getCarrionProduction(p2))
    cc <- carrion_consumption_ms(p2) * carrion_biomass(p2)
    expect_equal(cp, cc, tolerance = 1e-10)
})

test_that("tune_carrion_detritus sets detritus to steady state", {
    p2 <- tune_carrion_detritus(params)
    dp <- sum(getDetritusProduction(p2))
    dc <- detritus_consumption(p2)
    expect_equal(dp, dc, tolerance = 1e-10)
})

# ---- constant_dynamics ----

test_that("constant_dynamics returns the component value unchanged", {
    result <- constant_dynamics(params, params@initial_n_other, "carrion")
    expect_equal(result, params@initial_n_other$carrion)
})
