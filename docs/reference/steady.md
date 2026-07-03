# Drive a shelf model to steady state

Extends
[`mizer::steady()`](https://sizespectrum.org/mizer/reference/steady.html)
for `mizerShelf` objects: after the fish abundances have converged,
[`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md)
is called so that the carrion and detritus components are also at steady
state.

## Usage

``` r
# S3 method for class 'mizerShelf'
steady(
  params,
  t_max = 100,
  t_per = 1.5,
  dt = 0.1,
  tol = 0.1 * dt,
  return_sim = FALSE,
  ...
)
```

## Arguments

- params:

  A `mizerShelf` params object.

- t_max:

  The maximum number of years to run the simulation. Default is 100.

- t_per:

  The simulation is broken up into shorter runs of `t_per` years, after
  each of which we check for convergence. Default value is 1.5. This
  should be chosen as an odd multiple of the timestep `dt` in order to
  be able to detect period 2 cycles.

- dt:

  The time step to use in `project()`.

- tol:

  The simulation stops when the relative change in the egg production
  RDI over `t_per` years is less than `tol` for every species.

- return_sim:

  If TRUE, the function returns the MizerSim object holding the result
  of the simulation run, saved at intervals of `t_per`. If FALSE
  (default) the function returns a MizerParams object with the "initial"
  slots set to the steady state.

## Value

An updated `mizerShelf` object (or a `mizerShelfSim` when
`return_sim = TRUE`).
