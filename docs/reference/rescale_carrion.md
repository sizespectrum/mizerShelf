# Rescale carrion biomass without changing anything else

This multiplies the carrion biomass by a factor and divides the
interaction between all species and the carrion by the same factor, so
as to keep the total consumption of carrion unchanged. It also divides
the mass-specific rate of decomposition by the same factor so that the
total carrion decomposition rate stays the same.

## Usage

``` r
rescale_carrion(params, factor)
```

## Arguments

- params:

  A MizerParams object

- factor:

  A number

## Value

An updated MizerParams object
