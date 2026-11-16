params <- validParams(NWMed_params)

# ---- setRho ----

test_that("setRho returns MizerParams", {
    expect_s3_class(setRho(params), "MizerParams")
})

test_that("setRho sets rho from rho_carrion species parameter", {
    p2 <- setRho(params)
    n <- params@resource_params$n
    for (i in seq_len(nrow(params@species_params))) {
        expected_row <- params@species_params$rho_carrion[i] * params@w^n
        expect_equal(p2@other_params$carrion$rho[i, ], expected_row)
    }
})
