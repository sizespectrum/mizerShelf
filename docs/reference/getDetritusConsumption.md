# Get detritus consumption rates by consumer species

Returns a named vector with one entry for each species giving the rate
in grams/year at which that species consumes detritus.

## Usage

``` r
getDetritusConsumption(params, ...)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
  object.

- ...:

  Unused

## Value

A named vector with the consumption rates from all species

## See also

[`getDetritusProduction()`](https://sizespectrum.org/mizerShelf/reference/getDetritusProduction.md),
[`detritus_dynamics()`](https://sizespectrum.org/mizerShelf/reference/detritus_dynamics.md),
[`getCarrionConsumption()`](https://sizespectrum.org/mizerShelf/reference/getCarrionConsumption.md)
