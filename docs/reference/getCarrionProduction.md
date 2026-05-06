# Carrion production rate

This is the rate in grams/year at which the rest of the system produces
carrion biomass. The production comes from three sources:

## Usage

``` r
getCarrionProduction(
  params,
  n = params@initial_n,
  rates = getRates(params),
  ...
)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
  object.

- n:

  A matrix of current species abundances (species x size)

- rates:

  A list of rates as returned by
  [`getRates()`](https://sizespectrum.org/mizer/reference/getRates.html)

- ...:

  Unused

## Value

A vector with named entries "external", "gear_mort" and "discards", each
given the rate at which carrion biomass is produced by these sources in
grams per year.

## Details

1.  animals that have died by natural causes other than predation
    ("ext_mort"),

2.  animals killed by the fishing gear ("gear_mort"),

3.  discards from fishing ("discards").

The function returns a vector with the individual contributions. These
can be summed with [`sum()`](https://rdrr.io/r/base/sum.html) to get the
total production rate.

## See also

[`getCarrionConsumption()`](https://sizespectrum.org/mizerShelf/reference/getCarrionConsumption.md),
[`carrion_dynamics()`](https://sizespectrum.org/mizerShelf/reference/carrion_dynamics.md),
[`getDetritusProduction()`](https://sizespectrum.org/mizerShelf/reference/getDetritusProduction.md)
