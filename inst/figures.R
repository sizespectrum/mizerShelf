# script to produce figures in the paper

remotes::install_github("sizespectrum/mizerExperimental")
library(mizerExperimental)
remotes::install_github("gustavdelius/mizerShelf")
library(mizerShelf)
library(ggplot2)
library(patchwork)
library(dplyr)

params <- NWMed_params
# list of commercial species in Table 1 of the paper
target = c("Red mullet", "Striped red mullet", "Hake", "Angler fish", 
           "Poor cod", "Horse mackerel", "Blue whiting",
           "Horned octopus", "Shortfin squid")

# Size spectrum ----
plotSpectra(params, resource = FALSE, ylim = c(1e-10, NA))
ggsave("sizespectrum.pdf", width = 18, height = 9, units = "cm", scale = 1.2)

# Change selectivity ----
params_s <- NWMed_params
gear_params(params_s)["Hake, Main", c("l25", "l50")] <- c(18, 21.5)
gear_params(params_s)["Red mullet, Main", c("l25", "l50")] <- c(15.13, 17.28)
gear_params(params_s)["Striped red mullet, Main", c("l25", "l50")] <- c(15.13, 17.28)
gear_params(params_s)["Angler fish, Main", c("l25", "l50")] <- c(14, 18)
gear_params(params_s)["Poor cod, Main", c("l25", "l50")] <- c(8.1, 9.5)
gear_params(params_s)["Horse mackerel, Main", c("l25", "l50")] <- c(15.2, 17)
gear_params(params_s)["Blue whiting, Main", c("l25", "l50")] <- c(18.80, 22.25)
gear_params(params_s)["Shortfin squid, Main", c("l25", "l50")] <- c(14.2, 16.5)
gear_params(params_s)["Horned octopus, Main", c("l25", "l50")] <- c(14.20, 16.5)

sim_s <- project(params_s, t_max = 15)

p_bio <- mizer::plotBiomass(sim_s, species = target)

yield <- plotYield(sim_s, species = target, return_data = TRUE)
yield_initial <- data.frame(Year = rep(-1, length(target)),
                            Yield = getYield(params)[target],
                            Species = target)
p_yield <- plotDataFrame(rbind(yield_initial, yield), params,
                         ylab = "Yield [g/yr]")

p_bio + p_yield + plot_layout(guides = 'collect')

ggsave("selectvity.pdf", width = 18, height = 9, units = "cm", scale = 1.2)


# Landing obligation ----

params_d <- NWMed_params
species_params(params_d)["Hake", "discard"] <- 0
species_params(params_d)["Red mullet", "discard"] <- 0

sim_d <- project(params_d, t_max = 15)

# I think we decided not to include this boring graph in the paper
mizer::plotBiomass(sim_d, species = target)

# But may be this one?
plotBiomassRelative(sim_d, species = target)

# Effort reduction ----

params_e <- NWMed_params
initial_effort(params_e) <- 0.6
species_params(params_e)$gear_mort <- species_params(params)$gear_mort * 0.6

sim_e <- project(params_e, t_max = 15)

p_bio <- mizer::plotBiomass(sim_e, species = target)

yield <- plotYield(sim_e, species = target, return_data = TRUE)
yield_initial <- data.frame(Year = rep(-1, length(target)),
                            Yield = getYield(params)[target],
                            Species = target)
p_yield <- plotDataFrame(rbind(yield_initial, yield), params,
                         ylab = "Yield [g/yr]")

p_bio + p_yield + plot_layout(guides = 'collect')

ggsave("effort.pdf", width = 18, height = 9, units = "cm", scale = 1.2)

# In the following plot you will want to select a different set of species
# to see also what effect is on other components of the ecosystem.
plotBiomassRelative(sim_e, species = target)
