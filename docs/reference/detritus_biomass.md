# Detritus biomass

The detritus is internally described by a size spectrum in order to
reflect the fact that it is available to small predators but becomes
unavailable to large predators. The total biomass is thus obtained by
integrating over the abundance density multiplied by mass.

## Usage

``` r
detritus_biomass(params, n_pp = params@initial_n_pp)
```

## Arguments

- params:

  MizerParams

- n_pp:

  Detritus spectrum

## Value

The detritus biomass in grams
