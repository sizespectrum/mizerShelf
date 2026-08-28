# Rescale carrion and detritus biomass without changing anything else

A convenience wrapper that calls
[`rescale_carrion()`](https://sizespectrum.org/mizerShelf/reference/rescale_carrion.md)
and
[`rescale_detritus()`](https://sizespectrum.org/mizerShelf/reference/rescale_detritus.md).

## Usage

``` r
rescaleComponents(params, carrion_factor = 1, detritus_factor = 1)
```

## Arguments

- params:

  A MizerParams object

- carrion_factor:

  A number by which to multiply the carrion biomass

- detritus_factor:

  A number by which to multiply the detritus biomass

## Value

An updated MizerParams object
