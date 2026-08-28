# Drive a shelf model to steady state (superseded)

**\[superseded\]** Use
[`tuneSteadyState()`](https://sizespectrum.org/mizerShelf/reference/tuneSteadyState.md)
instead, which is the mizerShelf method that does the same job under the
name mizer now prefers. This method is kept so that scripts written
against the older
[`mizer::steady()`](https://sizespectrum.org/mizer/reference/superseded_steady.html)
keep working unchanged.

## Usage

``` r
# S3 method for class 'mizerShelf'
steady(
  params,
  t_max = 100,
  t_per = 1.5,
  dt = 0.1,
  t_save = dt,
  tol = 0.1 * dt,
  amplitude_tol = 0.01,
  amp_rel_tol = 0.01,
  extinction_threshold = 1e-06,
  return_sim = FALSE,
  preserve = c("reproduction_level", "erepro", "R_max"),
  progress_bar = TRUE,
  info_level = default_info_level(),
  method = c("euler", "predictor_corrector", "tr_bdf2")
)
```

## Arguments

- params:

  A `mizerShelf` params object.

- t_max:

  The maximum number of years to run the simulation. Default is 100.

- t_per:

  The interval in years at which convergence is checked, and hence also
  the interval at which the trajectory is saved when
  `return_sim = TRUE`. In
  [`projectUntilSettled()`](https://sizespectrum.org/mizer/reference/projectUntilSettled.html)
  these two roles have been separated into `t_check` and `t_save`.

- dt:

  The time step to use in `project()`.

- t_save:

  Has no effect. It briefly controlled how finely the biomass series
  used for limit-cycle detection was sampled; that series is now sampled
  at every time step, which is what its default `dt` gave.

- tol:

  The simulation stops when the relative change in the egg production
  RDI over t_per years is less than tol for every species.

- amplitude_tol:

  **\[experimental\]** The minimum relative biomass amplitude for a
  persistent oscillation to be reported as a limit cycle rather than
  treated as an (effectively steady) fixed point. This is a fraction of
  mean biomass and is kept separate from `distance_tol` (which measures
  convergence to a fixed point on a different scale). Default `0.01`.

- amp_rel_tol:

  **\[experimental\]** Maximum relative change of amplitude between
  successive periods for the cycle to count as settled. Default `0.01`.

- extinction_threshold:

  **\[experimental\]** A species is treated as going extinct, stopping
  the run, once its reproduction rate (RDD) falls below this fraction of
  its value at the start of the run. For example the default `1e-6`
  treats a species as extinct once its reproduction has collapsed to a
  millionth of its initial level. Because it is relative to the initial
  reproduction, a species that starts with zero reproduction is flagged
  immediately, and (in
  [`tuneSteadyState()`](https://sizespectrum.org/mizer/reference/tuneSteadyState.html),
  where reproduction is held constant) a healthy species is never
  flagged.

- return_sim:

  If TRUE, the function returns the MizerSim object holding the result
  of the simulation run, saved at intervals of `t_per`. If FALSE
  (default) the function returns a MizerParams object with the "initial"
  slots set to the steady state.

- preserve:

  **\[experimental\]** Specifies whether the `reproduction_level` should
  be preserved (default) or the maximum reproduction rate `R_max` or the
  reproductive efficiency `erepro`. See
  [`setBevertonHolt()`](https://sizespectrum.org/mizer/reference/setBevertonHolt.html)
  for an explanation of the `reproduction_level`.

- progress_bar:

  A shiny progress object to implement a progress bar in a shiny app.
  Default FALSE.

- info_level:

  Controls the amount of information messages that are shown. Higher
  levels lead to more messages, `info_level = 0` gives silence. The
  default is taken from the `mizer_info_level` option, see
  [`default_info_level()`](https://sizespectrum.org/mizer/reference/default_info_level.html).

- method:

  The numerical method to use for the consumer density update. See
  [`project()`](https://sizespectrum.org/mizer/reference/project.html).

## Value

An updated `mizerShelf` object (or a `mizerShelfSim` when
`return_sim = TRUE`).
