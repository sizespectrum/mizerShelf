# Plot the biomass of species and unstructured components through time

After running a projection, the biomass of each species and each
unstructured component can be plotted against time. The biomass is
calculated within user defined size limits (min_w, max_w, min_l, max_l,
see
[`get_size_range_array()`](https://sizespectrum.org/mizer/reference/get_size_range_array.html)).

## Usage

``` r
plotBiomass(
  sim,
  species = NULL,
  start_time,
  end_time,
  y_ticks = 6,
  ylim = c(NA, NA),
  total = FALSE,
  background = TRUE,
  highlight = NULL,
  return_data = FALSE,
  ...
)

plotlyBiomass(
  sim,
  species = NULL,
  start_time,
  end_time,
  y_ticks = 6,
  ylim = c(NA, NA),
  total = FALSE,
  background = TRUE,
  highlight = NULL,
  ...
)
```

## Arguments

- sim:

  An object of class MizerSim

- species:

  The species to be selected. Optional. By default all target species
  are selected. A vector of species names, or a numeric vector with the
  species indices, or a logical vector indicating for each species
  whether it is to be selected (TRUE) or not.

- start_time:

  The first time to be plotted. Default is the beginning of the time
  series.

- end_time:

  The last time to be plotted. Default is the end of the time series.

- y_ticks:

  The approximate number of ticks desired on the y axis.

- ylim:

  A numeric vector of length two providing lower and upper limits for
  the y axis. Use NA to refer to the existing minimum or maximum. Any
  values below 1e-20 are always cut off.

- total:

  A boolean value that determines whether the total biomass from all
  species is plotted as well. Default is FALSE.

- background:

  A boolean value that determines whether background species are
  included. Ignored if the model does not contain background species.
  Default is TRUE.

- highlight:

  Name or vector of names of the species to be highlighted.

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a data
frame with the four variables 'Year', 'Biomass', 'Species', 'Legend' is
returned.

## Examples

``` r
# \donttest{
NWMed_sim <- project(NWMed_params, t_max = 15, effort = 0.6)
#> Error in project(NWMed_params, t_max = 15, effort = 0.6): could not find function "project"
plotBiomass(NWMed_sim, species = c("Hake", "Red mullet"))
#> Error: object 'NWMed_sim' not found
plotBiomass(NWMed_sim, start_time = 5, end_time = 15)
#> Error: object 'NWMed_sim' not found

# Returning the data frame
fr <- plotBiomass(NWMed_sim, return_data = TRUE)
#> Error: object 'NWMed_sim' not found
str(fr)
#> Error: object 'fr' not found
# }
```
