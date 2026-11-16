# mizerShelf 1.2.0.9001

* Saved preview models retain their authored capability definitions and explicit
  removals when upgraded; older models still acquire the standard declarations.

* Re-registering capabilities after changing between the known `seMort` and
  `mizerMort` providers now selects the corresponding checked mortality
  decomposition, without retaining the previous provider's diagnostic.

* Adopts the mizer 3.4.0.9012 extension-capability preview. Saved shelf models
  record their equations, convergence measures, biomass measurement, joint
  tuning strategy, temporal support and structural/scaling transformations.
* `getBiomass()` now follows mizer's species-only default. Include detritus and
  carrion explicitly with `include = c("species", "resource", "component:carrion")`.
  These measurements retain stable IDs and plotting metadata for params and
  simulations. `plotBiomass()` still delegates to `plot(getBiomass())`.
* Species and grid changes update carrion encounter coefficients before
  dependent calculations. `addSpecies()` honours `steady`; explicit new-species
  `rho_carrion` values are used, while omitted values default to zero. The
  constructor retains its existing feeding-level-based initial derivation.
* Tuning uses a declared joint carrion/detritus budget strategy. Component
  freezing and final verification are handled by mizer; no post-hoc S3 repair
  or warning suppression remains. `tune_carrion_detritus()` remains available
  for explicitly balancing the two budgets at a fixed state in the known Shelf
  provider family with carrion as its only component. Composed or modified models
  need declared coordinated strategies through `tuneSteadyState()`.
* Removes the no-op `balance_detritus_dynamics()` callback. Resource parameter
  edits should use `setResource(..., balance = FALSE)`; the resource rate and
  capacity do not control detritus production. Use the joint tuning strategy
  or `tune_carrion_detritus()` when budget calibration is intended.
* Mortality plots use checked mortality decomposition. The bundled
  `NWMed_params` retains its historical `mizerMort` provider; upgrading it does
  not enable excess gear mortality. Models built with `seMort` retain that law
  and identify its extra gear mortality separately.
* New models initialise decomposition and external detritus inflow to zero,
  and the fraction of external mortality entering carrion to one; these
  required budget parameters were previously left unset by the constructor.
* Detritus biomass and consumption integrals honour both size-grid quadrature
  schemes. Complete effective rates are used throughout budget calculations.

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
