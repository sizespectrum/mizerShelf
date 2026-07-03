# Get carrion consumption rates

Returns a named vector with one entry for each species giving the rate
in grams/year at which that species consumes carrion, plus a `decompose`
entry for bacterial decomposition.

## Usage

``` r
getCarrionConsumption(params, ...)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
  object.

- ...:

  Unused

## Value

A named vector with the consumption rates from all species and
decomposition.

## See also

[`getCarrionProduction()`](https://sizespectrum.org/mizerShelf/reference/getCarrionProduction.md),
[`carrion_dynamics()`](https://sizespectrum.org/mizerShelf/reference/carrion_dynamics.md),
[`getDetritusConsumption()`](https://sizespectrum.org/mizerShelf/reference/getDetritusConsumption.md)
