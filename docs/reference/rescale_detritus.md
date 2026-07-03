# Rescale detritus biomass without changing detritus consumption

This multiplies the detritus abundance by a factor and divides the
interaction between all species and the detritus by the same factor, so
as to keep the total consumption of detritus unchanged.

## Usage

``` r
rescale_detritus(params, factor)
```

## Arguments

- params:

  A MizerParams object

- factor:

  A number

## Value

An updated MizerParams object
