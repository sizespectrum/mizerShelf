# Mass-specific carrion consumption rate

This includes both the consumption by fish and the decomposition by
smaller organisms.

## Usage

``` r
carrion_consumption_ms(params, n = params@initial_n, rates = getRates(params))
```

## Arguments

- params:

  MizerParams

- n:

  A matrix of current species abundances (species x size)

- rates:

  A list of rates as returned by
  [`getRates()`](https://sizespectrum.org/mizer/reference/getRates.html)

## Value

A number giving the mass-specific consumption rate in grams per year.

## Details

This mass-specific consumption rate is used in
[`carrion_dynamics()`](https://sizespectrum.org/mizerShelf/reference/carrion_dynamics.md)
to calculate the carrion biomass at the next time step. To get the
non-mass-specific consumption rate, use
[`getCarrionConsumption()`](https://sizespectrum.org/mizerShelf/reference/getCarrionConsumption.md).

The consumption rate by fish is determined by
`other_params(params)$carrion$rho` and the decomposition rate is given
by `other_params(params)$carrion$decompose`.
