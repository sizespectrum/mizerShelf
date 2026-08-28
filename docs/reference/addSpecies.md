# Add new species

**\[experimental\]**

Takes a
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
object and adds additional species with given parameters to the
ecosystem. It sets the initial values for these new species to their
steady-state solution in the given initial state of the existing
ecosystem. This will be close to the true steady state if the abundances
of the new species are sufficiently low. Hence the abundances of the new
species are set so that they are at most 1/100th of the resource power
law. Their reproductive efficiencies are set so as to keep them at that
low level.

## Usage

``` r
# S3 method for class 'mizerShelf'
addSpecies(params, species_params, ..., steady = FALSE)
```

## Arguments

- params:

  A mizer params object for the original system.

- species_params:

  Data frame with the species parameters of the new species we want to
  add to the system.

- ...:

  Passed to
  [`mizer::addSpecies()`](https://sizespectrum.org/mizer/reference/addSpecies.html),
  for example `interaction`, `gear_params` or `initial_effort`.

- steady:

  Ignored. This method always adds the new species with `steady = FALSE`
  because at that point the carrion encounter rate matrix `rho` still
  has the old species dimensions, which would make
  `steadySingleSpecies()` fail. The steady state for the new species is
  computed separately afterwards.

## Value

An object of type
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)

## Details

The resulting MizerParams object will use the same size grid where
possible, but if one of the new species needs a larger range of w
(either because a new species has an egg size smaller than those of
existing species or a maximum size larger than those of existing
species) then the grid will be expanded and all arrays will be enlarged
accordingly.

If any of the rate arrays of the existing species had been set by the
user to values other than those calculated as default from the species
parameters, then these will be preserved. Only the rates for the new
species will be calculated from their species parameters.

After adding the new species, the background species are not retuned and
the system is not run to steady state. This could be done with
[`tuneSteadyState()`](https://sizespectrum.org/mizerShelf/reference/tuneSteadyState.md).
The new species will have a reproduction level of 1/4, this can then be
changed with
[`setBevertonHolt()`](https://sizespectrum.org/mizer/reference/setBevertonHolt.html)

## See also

[`removeSpecies()`](https://sizespectrum.org/mizerShelf/reference/removeSpecies.md)

## Examples

``` r
params <- newTraitParams()
species_params <- data.frame(
    species = "Mullet",
    w_max = 173,
    w_mat = 15,
    beta = 283,
    sigma = 1.8,
    h = 30,
    a = 0.0085,
    b = 3.11
)
params <- addSpecies(params, species_params)
#> ℹ The species parameter data frame is missing a `w_inf` column. I am using the values from the `w_max` column instead. 
#> ℹ Using f0, h, lambda, kappa and the predation kernel to calculate gamma.
plotSpectra(params)
```
