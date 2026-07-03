# Detritus production rate

Returns a named vector with the rates at which different components of
the ecosystem produce detritus:

1.  biomass not assimilated by predators ("feces"),

2.  decomposing carrion ("carrion"),

3.  the pelagic zone ("external").

## Usage

``` r
getDetritusProduction(
  params,
  n = params@initial_n,
  n_other = params@initial_n_other,
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

- n_other:

  Other dynamic components. Only `n_other$carrion` is used.

- rates:

  A list of rates as returned by
  [`getRates()`](https://sizespectrum.org/mizer/reference/getRates.html)

- ...:

  Unused

## Value

A vector with named entries "external", "feces" and "carrion", giving
the rates at which carrion biomass is produced by these sources in grams
per year.

## Details

The function returns a vector with the individual contributions. These
can be summed with [`sum()`](https://rdrr.io/r/base/sum.html) to get the
total production rate.
