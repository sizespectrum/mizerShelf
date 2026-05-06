# Total mortality rate in the shelf ecosystem model

This function replaces the usual
[`mizerMort()`](https://sizespectrum.org/mizer/reference/mizerMort.html)
function and returns the sum of the usual mortality and the excess gear
mortality calculated with
[`gearMort()`](https://sizespectrum.org/mizerShelf/reference/gearMort.md).

## Usage

``` r
seMort(params, n, n_pp, n_other, t, f_mort, pred_mort, ...)
```

## Arguments

- params:

  A MizerParams object

- n:

  A matrix of species abundances (species x size).

- n_pp:

  A vector of the resource abundance by size

- n_other:

  A list of abundances for other dynamical components of the ecosystem

- t:

  The time for which to do the calculation (Not used by standard mizer
  rate functions but useful for extensions with time-dependent
  parameters.)

- f_mort:

  A two dimensional array (species x size) with the fishing mortality

- pred_mort:

  A two dimensional array (species x size) with the predation mortality

- ...:

  Unused

## Value

A named two dimensional array (species x size) with the total mortality
rates.
