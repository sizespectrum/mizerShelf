# Plot yield minus discards

Plot yield minus discards

## Usage

``` r
plotYieldMinusDiscards(
  sim,
  sim2,
  species = NULL,
  total = FALSE,
  log = TRUE,
  highlight = NULL,
  return_data = FALSE,
  ...
)
```

## Arguments

- sim:

  An `MizerSim` object.

- sim2:

  Optional second `MizerSim` object.

- species:

  Species to plot.

- total:

  Logical; should total yield be included in plot?

- log:

  Logical; should the y-axis be log-transformed?

- highlight:

  Name of species to highlight in plot.

- return_data:

  Logical; should the underlying data be returned?

- ...:

  Other arguments passed to
  [`getYield`](https://sizespectrum.org/mizer/reference/getYield.html).

## Value

If `return_data = TRUE`, a data frame of the underlying data is
returned.

## Examples

``` r
# \donttest{
sim <- project(NWMed_params, t_max = 3)
plotYieldMinusDiscards(sim)

# }
```
