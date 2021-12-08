# Silvia and Francesc have made some parameter changes. I will read them
# from their excel file into the latest params object

source("init.R")
library(googlesheets4)

catch <- readRDS("catch.rds")
params <- readRDS("params191121.rds")
p <- params
sp_old <- sp <- p@species_params
gp_old <- gp <- p@gear_params

sp_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1mXJ4BMeO6V95-B58UqQ-ZxqHcQuTXQnN5_Wa17zC7CE/edit?usp=sharing",
                       na = "NA")
compare(sp_sheet$species, sp$species)

# Check consistency between lengths and weights ----
plot_diff(sp_sheet$w_inf, w(sp_sheet$adult_max_length))
plot_diff(sp_sheet$w_mat, w(sp_sheet$l_mat))

# Transfer information to params object ----

plot_diff(sp_old$w_min, sp_sheet$w_min)
sp$w_min <- sp_sheet$w_min

plot_diff(sp_old$w_mat, sp_sheet$w_mat)
sp$species[rel_diff(sp_old$w_mat, sp_sheet$w_mat) > 0.1]
sp$w_mat <- sp_sheet$w_mat
plot_diff(sp_old$w_mat25, sp_sheet$w_mat25)
sp$species[rel_diff(sp_old$w_mat25, sp_sheet$w_mat25) > 0.1]
sp$w_mat25 <- sp_sheet$w_mat25

plot_diff(sp_old$w_inf, sp_sheet$w_inf)
sp$species[rel_diff(sp_old$w_inf, sp_sheet$w_inf) > 0.1]
sp$w_inf <- sp_sheet$w_inf
for (i in 1:nrow(sp)) {
    p@initial_n[i, p@w > sp$w_inf[[i]]] <- 0
}

plot_diff(sp_old$k_vb, sp_sheet$k_vb)
sp$species[rel_diff(sp_old$k_vb, sp_sheet$k_vb) > 0.1]
sp$k_vb <- sp_sheet$k_vb

plot_diff(sp_old$t0, sp_sheet$t0)
sp$species[rel_diff(sp_old$t0, sp_sheet$t0) > 0.1]
sp$t0 <- sp_sheet$t0

plot_diff(sp_old$a, sp_sheet$a)
sp$a <- sp_sheet$a

plot_diff(sp_old$b, sp_sheet$b)
sp$b <- sp_sheet$b

compare(as.character(gp$species), sp_sheet$species)
gp$sel_func[sp_sheet$`selectivity model` == "Knife edge (length)"] <-
    "knife_edge"
gp$sel_func[sp_sheet$`selectivity model` == "Sigmoidal (length)"] <-
    "sigmoid_length"

plot_diff(gp_old$l50, sp_sheet$l50)
plot_diff(gp_old$l25, sp_sheet$l25)
gp$knife_edge_size <- sp_sheet$l50

compare(sp_old$discard, sp_sheet$discard)
sp$discard <- sp_sheet$discard
compare(sp_old$gearMort, sp_sheet$gear_mortality)
# deal with gear mortality later

sp$biomass_cutoff <- sp_sheet$biomass_cutoff
sp$biomass_observed <- sp_sheet$biomass_observed
sp$number_cutoff <- sp_sheet$number_cutoff
sp$number_observed <- sp_sheet$number_observed
sp$yield_observed <- sp_sheet$yield_observed

# sp$interaction_resource <- sp_sheet$detQ
# Let everyone feed on detritus
sp$interaction_resource <- 1
sp_old$rho_carrion[sp_sheet$scavQ == 0]
sp_old$rho_carrion[sp_sheet$scavQ == 1]

species_params(p) <- sp
p <- setRho(p)
for (i in 1:nrow(sp)) {
    if (p@w_min_idx[i] < params@w_min_idx[i]) {
        p@initial_n[i, p@w_min_idx[i]:params@w_min_idx[i]] <-
            params@initial_n[i, params@w_min_idx[i]]
    }
}

# theta <- read_sheet("https://docs.google.com/spreadsheets/d/1fC5kge_GkrhqFPe0qtX-zvU5hRIZhwUpESWh49e8XYw",
#                     col_names = FALSE)
# theta <- t(as.matrix(theta))
# p <- setInteraction(p, interaction = theta)

# Tune ----
controls <- list("abundance", "predation", "rho", "fishing", "reproduction", "other",
                 "interaction")
p <- tuneParams(p, catch = catch, controls = controls)

saveRDS(p, file = "params071221.rds")
