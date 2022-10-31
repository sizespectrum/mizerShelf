test_that("getCarrionConsumption and carrion_consumption_ms agree", {
  expect_equal(sum(getCarrionConsumption(NWMed_params)),
               carrion_consumption_ms(NWMed_params) * carrion_biomass(NWMed_params))
})

test_that("getDetritusConsumption and detritus_consumption agree", {
    expect_equal(sum(getDetritusConsumption(NWMed_params)),
                 detritus_consumption(NWMed_params))
})
