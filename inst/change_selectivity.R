source("init.R")
params <- readRDS("params191121.rds")

params <- setBevertonHolt(params, reproduction_level = 1/2)

params@gear_params$l50[16:25] <-
    c(18.500, 17.280, 17.280, 21.500,18.000, 9.500, 17.000,
      16.500, 22.250, 16.500)
params@gear_params$l25[16:25] <-
    c(16.600, 15.130, 15.130, 19.750, 14.000, 8.100, 15.200,
      14.200, 19.800, 14.200)
params <- setFishing(params)

sim <- project(params, t_max = 10, t_save = 0.5)
plotBiomass(sim)
plotlyYield(sim)
