# Tune carrion and detritus to steady state

This first sets the rate of decomposition of carrion so that for the
given abundances, the carrion is at steady state. It then sets the rate
at which detritus flows in from external sources (e.g. the pelagic zone)
so that for the given abundances the detritus is at steady state.

## Usage

``` r
tune_carrion_detritus(params)
```

## Arguments

- params:

  A MizerParams object

## Value

An updated MizerParams object

## Details

Note that this reaches a steady state by changing *parameters* — the
carrion decomposition rate and the external detritus inflow — so that
the stored abundances become a fixed point. That is what is needed after
a steady-state finder that holds the components fixed, and it is why
[`tuneSteadyState()`](https://sizespectrum.org/mizerShelf/reference/tuneSteadyState.md)
and
[`steady()`](https://sizespectrum.org/mizerShelf/reference/steady.md)
have mizerShelf methods that call this function:

- [`tuneSteadyState()`](https://sizespectrum.org/mizerShelf/reference/tuneSteadyState.md)
  and
  [`steady()`](https://sizespectrum.org/mizerShelf/reference/steady.md)
  hold the carrion at its stored value while they work, so the
  mizerShelf methods call this afterwards. They leave the components
  exactly at a fixed point.

- [`mizer::findSteadyState()`](https://sizespectrum.org/mizer/reference/findSteadyState.html)
  with the default `solver = "project"`, and
  [`mizer::projectUntilSettled()`](https://sizespectrum.org/mizer/reference/projectUntilSettled.html),
  advance the carrion like every other state variable, so they settle it
  themselves, to within their convergence tolerance, and leave the
  parameters alone. They have no mizerShelf method and do not need one.
  Calling this function after one of them would re-solve for parameters
  the model has just been settled against.

- [`mizer::findSteadyState()`](https://sizespectrum.org/mizer/reference/findSteadyState.html)
  with `solver = "newton"` declines a model with custom resource
  dynamics, which includes every mizerShelf model, and says so.

## See also

[`tuneSteadyState()`](https://sizespectrum.org/mizerShelf/reference/tuneSteadyState.md),
[`carrion_lifetime()`](https://sizespectrum.org/mizerShelf/reference/carrion_lifetime.md),
[`detritus_lifetime()`](https://sizespectrum.org/mizerShelf/reference/detritus_lifetime.md)
