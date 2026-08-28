# Balance the detritus resource

Mizer calls a `balance_<resource_dynamics>()` function whenever it
restores a resource, to adjust the resource rate or the resource
capacity so that the stored resource abundance is a steady state of the
resource dynamics. It warns when no such function exists, because a
custom resource would then silently come back off its own fixed point.

## Usage

``` r
balance_detritus_dynamics(
  params,
  resource_rate = NULL,
  resource_capacity = NULL
)
```

## Arguments

- params:

  A `mizerShelf` params object.

- resource_rate:

  The resource rate requested by the caller, or `NULL` to keep the
  stored one.

- resource_capacity:

  The resource capacity requested by the caller, or `NULL` to keep the
  stored one.

## Value

A list with entries `resource_rate` and `resource_capacity`.

## Details

[`detritus_dynamics()`](https://sizespectrum.org/mizerShelf/reference/detritus_dynamics.md)
reads neither the resource rate `rr_pp` nor the resource capacity
`cc_pp`: the detritus is held at a fixed power law whose total biomass
follows a production-minus-consumption balance. There is therefore
nothing for mizer to balance, and this function returns both unchanged.
What does make the stored detritus abundance a steady state is the
external detritus inflow `other_params(params)$detritus$external`, which
is set by
[`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md).

## See also

[`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md),
[`detritus_dynamics()`](https://sizespectrum.org/mizerShelf/reference/detritus_dynamics.md)
