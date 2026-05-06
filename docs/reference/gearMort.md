# Excess gear mortality rate

The excess gear mortality rate is the difference between the total gear
mortality (as given by the species parameter `gear_mort`) and the
fishing mortality) if this is positive. If the total gear mortality does
not exceed the fishing mortality then the excess gear mortality is zero.

## Usage

``` r
gearMort(params, f_mort)
```

## Arguments

- params:

  A MizerParams object

- f_mort:

  The array of fishing mortality rates as calculated by `getFMort()`.

## Value

A named two dimensional array (species x size) with the excess gear
mortality rates.
