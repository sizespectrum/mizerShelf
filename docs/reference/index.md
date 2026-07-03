# Package index

## Detritus

- [`detritus_biomass()`](https://sizespectrum.org/mizerShelf/reference/detritus_biomass.md)
  : Detritus biomass
- [`detritus_consumption()`](https://sizespectrum.org/mizerShelf/reference/detritus_consumption.md)
  : Detritus consumption rate
- [`detritus_dynamics()`](https://sizespectrum.org/mizerShelf/reference/detritus_dynamics.md)
  : Detritus dynamics
- [`detritus_lifetime()`](https://sizespectrum.org/mizerShelf/reference/detritus_lifetime.md)
  [`` `detritus_lifetime<-`() ``](https://sizespectrum.org/mizerShelf/reference/detritus_lifetime.md)
  : Expected detritus lifetime
- [`getDetritusConsumption()`](https://sizespectrum.org/mizerShelf/reference/getDetritusConsumption.md)
  : Get detritus consumption rates by consumer species
- [`getDetritusProduction()`](https://sizespectrum.org/mizerShelf/reference/getDetritusProduction.md)
  : Detritus production rate
- [`newDetritusCarrionParams()`](https://sizespectrum.org/mizerShelf/reference/newDetritusCarrionParams.md)
  : Create new mizer model with detritus and carrion components
- [`plotDetritusConsumption()`](https://sizespectrum.org/mizerShelf/reference/plotDetritusConsumption.md)
  : Plot detritus consumption rates
- [`plotDetritusProduction()`](https://sizespectrum.org/mizerShelf/reference/plotDetritusProduction.md)
  : Plot detritus production rates
- [`rescale_detritus()`](https://sizespectrum.org/mizerShelf/reference/rescale_detritus.md)
  : Rescale detritus biomass without changing detritus consumption
- [`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md)
  : Tune carrion and detritus to steady state

## Carrion

- [`carrion_biomass()`](https://sizespectrum.org/mizerShelf/reference/carrion_biomass.md)
  : Carrion biomass
- [`carrion_consumption_ms()`](https://sizespectrum.org/mizerShelf/reference/carrion_consumption_ms.md)
  : Mass-specific carrion consumption rate
- [`carrion_dynamics()`](https://sizespectrum.org/mizerShelf/reference/carrion_dynamics.md)
  : Carrion dynamics
- [`carrion_human_origin()`](https://sizespectrum.org/mizerShelf/reference/carrion_human_origin.md)
  : Proportion of carrion production that is of human origin
- [`carrion_lifetime()`](https://sizespectrum.org/mizerShelf/reference/carrion_lifetime.md)
  [`` `carrion_lifetime<-`() ``](https://sizespectrum.org/mizerShelf/reference/carrion_lifetime.md)
  : Expected carrion lifetime
- [`getCarrionConsumption()`](https://sizespectrum.org/mizerShelf/reference/getCarrionConsumption.md)
  : Get carrion consumption rates
- [`getCarrionProduction()`](https://sizespectrum.org/mizerShelf/reference/getCarrionProduction.md)
  : Carrion production rate
- [`newDetritusCarrionParams()`](https://sizespectrum.org/mizerShelf/reference/newDetritusCarrionParams.md)
  : Create new mizer model with detritus and carrion components
- [`plotCarrionConsumption()`](https://sizespectrum.org/mizerShelf/reference/plotCarrionConsumption.md)
  : Plot carrion consumption rates
- [`plotCarrionProduction()`](https://sizespectrum.org/mizerShelf/reference/plotCarrionProduction.md)
  : Plot carrion production rates
- [`rescale_carrion()`](https://sizespectrum.org/mizerShelf/reference/rescale_carrion.md)
  : Rescale carrion biomass without changing anything else
- [`tune_carrion_detritus()`](https://sizespectrum.org/mizerShelf/reference/tune_carrion_detritus.md)
  : Tune carrion and detritus to steady state

## Plots

- [`plotCarrionConsumption()`](https://sizespectrum.org/mizerShelf/reference/plotCarrionConsumption.md)
  : Plot carrion consumption rates
- [`plotCarrionProduction()`](https://sizespectrum.org/mizerShelf/reference/plotCarrionProduction.md)
  : Plot carrion production rates
- [`plotDeath()`](https://sizespectrum.org/mizerShelf/reference/plotDeath.md)
  [`plotlyDeath()`](https://sizespectrum.org/mizerShelf/reference/plotDeath.md)
  : Plot death rates
- [`plotDetritusConsumption()`](https://sizespectrum.org/mizerShelf/reference/plotDetritusConsumption.md)
  : Plot detritus consumption rates
- [`plotDetritusProduction()`](https://sizespectrum.org/mizerShelf/reference/plotDetritusProduction.md)
  : Plot detritus production rates
- [`plotYieldMinusDiscards()`](https://sizespectrum.org/mizerShelf/reference/plotYieldMinusDiscards.md)
  : Plot yield minus discards

## Mortality

- [`gearMort()`](https://sizespectrum.org/mizerShelf/reference/gearMort.md)
  : Excess gear mortality rate
- [`seMort()`](https://sizespectrum.org/mizerShelf/reference/seMort.md)
  : Total mortality rate in the shelf ecosystem model

## Others

- [`mizerShelf-class`](https://sizespectrum.org/mizerShelf/reference/mizerShelf-class.md)
  : mizerShelf marker classes
- [`encounter_contribution()`](https://sizespectrum.org/mizerShelf/reference/encounter_contribution.md)
  : Contribution of unstructured components to the encounter rate

## Extended mizer methods

- [`getBiomass(`*`<mizerShelf>`*`)`](https://sizespectrum.org/mizerShelf/reference/getBiomass.md)
  : Get biomass of species and components for a shelf model
- [`steady(`*`<mizerShelf>`*`)`](https://sizespectrum.org/mizerShelf/reference/steady.md)
  : Drive a shelf model to steady state
- [`removeSpecies(`*`<mizerShelf>`*`)`](https://sizespectrum.org/mizerShelf/reference/removeSpecies.md)
  : Remove some species from a shelf model
- [`addSpecies(`*`<mizerShelf>`*`)`](https://sizespectrum.org/mizerShelf/reference/addSpecies.md)
  **\[experimental\]** : Add new species
- [`scaleModel(`*`<mizerShelf>`*`)`](https://sizespectrum.org/mizerShelf/reference/scaleModel.md)
  : Scale model parameters

## Example model

- [`NWMed_catch`](https://sizespectrum.org/mizerShelf/reference/NWMed_catch.md)
  : Catch size distribution for the NW Mediterranean shelf around Blanes
- [`NWMed_params`](https://sizespectrum.org/mizerShelf/reference/NWMed_params.md)
  : MizerParams object for the NW Mediterranean shelf model
