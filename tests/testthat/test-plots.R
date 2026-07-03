params <- validParams(NWMed_params)

# ---- plotCarrionConsumption ----

test_that("plotCarrionConsumption returns a ggplot object", {
    expect_s3_class(plotCarrionConsumption(params), "ggplot")
})

# ---- plotCarrionProduction ----

test_that("plotCarrionProduction returns a ggplot object", {
    expect_s3_class(plotCarrionProduction(params), "ggplot")
})

# ---- plotDetritusConsumption ----

test_that("plotDetritusConsumption returns a ggplot object", {
    expect_s3_class(plotDetritusConsumption(params), "ggplot")
})

# ---- plotDetritusProduction ----

test_that("plotDetritusProduction returns a ggplot object", {
    expect_s3_class(plotDetritusProduction(params), "ggplot")
})

# ---- plotBiomass ----

test_that("plotBiomass with return_data returns a data frame", {
    sim <- project(params, t_max = 2, t_per = 1)
    df <- plotBiomass(sim, return_data = TRUE)
    expect_s3_class(df, "data.frame")
})

test_that("plotBiomass data includes Detritus", {
    sim <- project(params, t_max = 2, t_per = 1)
    df <- plotBiomass(sim, return_data = TRUE)
    expect_true("Detritus" %in% df$Species)
})

test_that("plotBiomass data includes carrion", {
    sim <- project(params, t_max = 2, t_per = 1)
    df <- plotBiomass(sim, return_data = TRUE)
    expect_true("carrion" %in% df$Species)
})

test_that("plotBiomass errors when start_time >= end_time", {
    sim <- project(params, t_max = 2, t_per = 1)
    expect_error(plotBiomass(sim, start_time = 2, end_time = 1),
                 "tlim\\[1\\] must be less than tlim\\[2\\]")
})

test_that("plotBiomass returns a ggplot object", {
    sim <- project(params, t_max = 2, t_per = 1)
    expect_s3_class(plotBiomass(sim), "ggplot")
})

# ---- plotDeath ----

test_that("plotDeath with return_data returns a data frame", {
    df <- plotDeath(params, species = "Hake", return_data = TRUE)
    expect_s3_class(df, "data.frame")
})

test_that("plotDeath data has expected columns", {
    df <- plotDeath(params, species = "Hake", return_data = TRUE)
    expect_named(df, c("w", "value", "Cause", "Prey"))
})

test_that("plotDeath data includes Gear cause", {
    df <- plotDeath(params, species = "Hake", return_data = TRUE)
    expect_true("Gear" %in% df$Cause)
})

test_that("plotDeath returns a ggplot object", {
    expect_s3_class(plotDeath(params, species = "Hake"), "ggplot")
})

# ---- plotYieldMinusDiscards ----

test_that("plotYieldMinusDiscards with return_data returns a data frame", {
    sim <- project(params, t_max = 2, t_per = 1)
    df <- plotYieldMinusDiscards(sim, return_data = TRUE)
    expect_s3_class(df, "data.frame")
})

test_that("plotYieldMinusDiscards data has Year, Yield, Species columns", {
    sim <- project(params, t_max = 2, t_per = 1)
    df <- plotYieldMinusDiscards(sim, return_data = TRUE)
    expect_true(all(c("Year", "Yield", "Species") %in% names(df)))
})

test_that("plotYieldMinusDiscards returns a ggplot object", {
    sim <- project(params, t_max = 2, t_per = 1)
    expect_s3_class(plotYieldMinusDiscards(sim), "ggplot")
})
