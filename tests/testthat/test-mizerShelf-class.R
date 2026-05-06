params <- validParams(NWMed_params)
sim <- project(params, t_max = 2, t_per = 1)

n_species <- nrow(species_params(params))

# ---- getBiomass.mizerShelf ----

test_that("getBiomass.mizerShelf returns a named numeric vector", {
    b <- getBiomass(params)
    expect_true(is.numeric(b))
    expect_null(dim(b))
})

test_that("getBiomass.mizerShelf includes all species", {
    b <- getBiomass(params)
    expect_true(all(species_params(params)$species %in% names(b)))
})

test_that("getBiomass.mizerShelf includes Detritus", {
    b <- getBiomass(params)
    expect_true("Detritus" %in% names(b))
})

test_that("getBiomass.mizerShelf includes carrion", {
    b <- getBiomass(params)
    expect_true("carrion" %in% names(b))
})

test_that("getBiomass.mizerShelf has species + Detritus + carrion entries", {
    b <- getBiomass(params)
    expect_length(b, n_species + 2L)
})

test_that("getBiomass.mizerShelf Detritus value matches manual calculation", {
    b <- getBiomass(params)
    expected <- sum(params@initial_n_pp * params@dw_full * params@w_full)
    expect_equal(b[["Detritus"]], expected)
})

test_that("getBiomass.mizerShelf carrion value matches initial_n_other", {
    b <- getBiomass(params)
    expect_equal(b[["carrion"]], params@initial_n_other$carrion)
})

# ---- getBiomass.mizerShelfSim ----

test_that("getBiomass.mizerShelfSim includes Detritus column", {
    b <- getBiomass(sim)
    expect_true("Detritus" %in% dimnames(b)[[2]])
})

test_that("getBiomass.mizerShelfSim includes carrion column", {
    b <- getBiomass(sim)
    expect_true("carrion" %in% dimnames(b)[[2]])
})

test_that("getBiomass.mizerShelfSim has correct number of columns", {
    b <- getBiomass(sim)
    expect_equal(ncol(b), n_species + 2L)
})

test_that("getBiomass.mizerShelfSim has correct number of rows", {
    b <- getBiomass(sim)
    expect_equal(nrow(b), dim(sim@n)[1])
})

test_that("getBiomass params and sim final time step are consistent", {
    b_params <- getBiomass(params)
    b_sim <- getBiomass(sim)
    # The sim was projected from params so initial time step should match params
    expect_equal(unname(b_sim[1, "Detritus"]), unname(b_params[["Detritus"]]))
})
