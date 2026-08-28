# Drive a shelf model to steady state

Extends
[`mizer::tuneSteadyState()`](https://sizespectrum.org/mizer/reference/tuneSteadyState.html)
for `mizerShelf` objects: after the consumer abundances have converged,
[`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md)
is called so that the carrion and detritus components are at steady
state too.

## Usage

``` r
# S3 method for class 'mizerShelf'
tuneSteadyState(
  params,
  solver = c("project", "newton"),
  effort = params@initial_effort,
  preserve = c("reproduction_level", "erepro", "R_max"),
  info_level = default_info_level(),
  ...
)
```

## Arguments

- params:

  A `mizerShelf` params object.

- solver:

  The solver to use: `"project"` to run the dynamics until they settle,
  `"newton"` to solve the steady-state equation directly. See *Choosing
  a solver*.

- effort:

  The fishing effort to use throughout. By default the initial effort
  stored in `params`.

- preserve:

  **\[experimental\]** Specifies whether the `reproduction_level` should
  be preserved (default) or the maximum reproduction rate `R_max` or the
  reproductive efficiency `erepro`. See
  [`setBevertonHolt()`](https://sizespectrum.org/mizer/reference/setBevertonHolt.html)
  for an explanation of the `reproduction_level`.

- info_level:

  Controls the amount of information messages that are shown. Higher
  levels lead to more messages, `info_level = 0` gives silence. The
  default is taken from the `mizer_info_level` option, see
  [`default_info_level()`](https://sizespectrum.org/mizer/reference/default_info_level.html).

- ...:

  Passed to
  [`mizer::tuneSteadyState()`](https://sizespectrum.org/mizer/reference/tuneSteadyState.html),
  for example `t_max`, `dt`, `distance_tol` or `method`.

## Value

An updated `mizerShelf` object.

## Details

Mizer's own steady-state machinery covers the consumers and the resource
and holds any component registered with
[`mizer::setComponent()`](https://sizespectrum.org/mizer/reference/setComponent.html)
at its stored value, which is why it warns about the carrion component.
This method takes responsibility for that component, so the warning is
suppressed here.

## See also

[`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md)
