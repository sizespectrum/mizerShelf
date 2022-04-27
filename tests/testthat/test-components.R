test_that("getCarrionConsumption and carrion_consumption_ms agree", {
  expect_equal(sum(getCarrionConsumption(params)),
               carrion_consumption_ms(params) * carrion_biomass(params))
})

test_that("getDetritusConsumption and detritus_consumption agree", {
    expect_equal(sum(getDetritusConsumption(params)),
                 detritus_consumption(params))
})
