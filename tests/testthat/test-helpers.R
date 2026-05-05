params <- validParams(NWMed_params)

# ---- removeSpecies ----

test_that("removeSpecies reduces species count by 1", {
    p2 <- removeSpecies(params, "Hake")
    expect_equal(nrow(species_params(p2)), nrow(species_params(params)) - 1)
})

test_that("removeSpecies removes the specified species", {
    p2 <- removeSpecies(params, "Hake")
    expect_false("Hake" %in% species_params(p2)$species)
})

test_that("removeSpecies also removes species row from carrion rho", {
    p2 <- removeSpecies(params, "Hake")
    expect_equal(nrow(p2@other_params$carrion$rho), nrow(species_params(p2)))
})

test_that("removeSpecies keeps all other species in rho", {
    p2 <- removeSpecies(params, "Hake")
    remaining <- setdiff(rownames(params@other_params$carrion$rho), "Hake")
    expect_equal(rownames(p2@other_params$carrion$rho), remaining)
})

# ---- setRho ----

test_that("setRho returns MizerParams", {
    expect_s4_class(setRho(params), "MizerParams")
})

test_that("setRho sets rho from rho_carrion species parameter", {
    p2 <- setRho(params)
    n <- params@resource_params$n
    for (i in seq_len(nrow(params@species_params))) {
        expected_row <- params@species_params$rho_carrion[i] * params@w^n
        expect_equal(p2@other_params$carrion$rho[i, ], expected_row)
    }
})
