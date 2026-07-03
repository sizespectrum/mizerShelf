# Detritus consumption rate

An internal helper function. This returns the total detritus consumption
rate and is used in
[`detritus_dynamics()`](https://sizespectrum.org/mizerShelf/reference/detritus_dynamics.md)
to calculate the detritus abundance at the next time step. To get the
consumption rate split up by consumer species, use
[`getDetritusConsumption()`](https://sizespectrum.org/mizerShelf/reference/getDetritusConsumption.md).

## Usage

``` r
detritus_consumption(
  params,
  n_pp = params@initial_n_pp,
  rates = getRates(params)
)
```

## Arguments

- params:

  MizerParams

- n_pp:

  Detritus spectrum

- rates:

  A list of rates as returned by
  [`getRates()`](https://sizespectrum.org/mizer/reference/getRates.html)

## Value

A number giving the consumption rate in grams per year.
