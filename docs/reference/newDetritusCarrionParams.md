# Create new mizer model with detritus and carrion components

Create new mizer model with detritus and carrion components

## Usage

``` r
newDetritusCarrionParams(
  species_params,
  w_min_detritus = NA,
  w_max_detritus = 1,
  n = 0.7,
  ...
)
```

## Arguments

- species_params:

  A species parameter data frame

- w_min_detritus:

  Minimum size of detritus in grams

- w_max_detritus:

  Maximum size of detritus in grams

- n:

  Growth exponent (also used as metabolic exponent p)

- ...:

  Extra parameters to be passed to
  [`newMultispeciesParams()`](https://sizespectrum.org/mizer/reference/newMultispeciesParams.html)

## Value

A `mizerShelf` object
