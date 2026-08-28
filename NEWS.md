# mizerShelf 1.1.0

Brings the package up to date with mizer 3.3.0.9000, which mizerShelf now
requires.

## Steady state

- New `tuneSteadyState()` method for `mizerShelf` objects. It is the mizerShelf
  method under the name mizer now prefers for what `steady()` did, and like the
  `steady()` method it calls `tune_carrion_detritus()` once the consumers have
  converged. The `steady()` method is kept, and is now labelled superseded.

- The carrion and detritus calculations no longer assume they are given the
  complete list of rates. Mizer's steady-state diagnostics call the resource and
  component dynamics with only the rates that the resource mortality depends on,
  which leaves out the fishing mortality that `getCarrionProduction()` needs.
  It used to fail with "non-conformable arguments", which `isSteady()` caught
  and reported as a model that is not at its steady state. Whatever is missing
  is now filled in, so `getSteadyResidual()`, `isSteady()` and the steady-state
  line of `summary()` work on a shelf model. `NWMed_params` is correctly
  recognised as being at its steady state.

- New `balance_detritus_dynamics()` tells mizer that there is nothing to balance
  when it restores the detritus resource. `detritus_dynamics()` reads neither
  the resource rate nor the resource capacity, so mizer's warning that it could
  not rebalance them was a false alarm. What makes the stored detritus abundance
  a steady state is the external inflow set by `tune_carrion_detritus()`.

- `tuneSteadyState()` no longer passes on mizer's warning that the carrion
  component is held fixed throughout the search, because the mizerShelf method
  takes responsibility for that component itself.

## Second-order bin-averaging

- The integrals over the consumer size spectrum in the carrion and detritus
  calculations now use `mizer::bin_average_weight()`, so they apply the
  quadrature scheme the model is actually on. Results are unchanged on the
  default scheme; a model with `second_order_w(params)$bin_average` switched on
  now gets carrion and detritus rates that are correct to second order.

## Other changes

- `plotDeath()` uses `mizer::finalParams()` in place of the deprecated
  `setInitialValues()`.

- `plotYieldMinusDiscards(sim, sim2)` works again. It took the discard fraction
  off the long data frame that `plotYield(return_data = TRUE)` returns rather
  than off the yield array, which has not been a numeric matrix for some time.

- The `steady()` method takes the full argument list of the generic, which has
  grown `t_save`, `amplitude_tol`, `amp_rel_tol` and `extinction_threshold`.

- The "Exploring scenarios" vignette follows the extra `Legend` column that
  `plotYield(return_data = TRUE)` now returns, and the model description uses
  `reproduction_level()` in place of the deprecated `getReproductionLevel()`.

- Replaced obsolete session extension registration and dynamic S4 marker classes with mizer's simpler S3 extension mechanism. `NWMed_params` is now stored as an S3 object with extension class vector `c("mizerShelf", "MizerParams")`, removing the need for a load hook or active binding.

- `newDetritusCarrionParams()` records the extension with
  `mizer::recordExtension()` and sets the S3 class vector via
  `mizer::coerceToExtensionClass()`. The object carries a mizerShelf version
  stamp and requirement, and records the extensions actually applied to it.

- The agent configuration files are excluded from the package build, so
  `R CMD check` no longer reports them as non-standard top-level and hidden
  files.

# mizerShelf 1.0.2

Version accompanying de Juan, Delius & Maynou (2023),
<https://doi.org/10.1016/j.fishres.2023.106764>.
