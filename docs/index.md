# mizerShelf

This is an extension package for the mizer package
(<https://sizespectrum.org/mizer/>) that makes it easy to set up a mizer
model with a detritus and a carrion component, thereby allowing for more
realistic modelling of the benthic component of fisheries on the
continental shelf.

This package was developed to support the creation and exploration of a
model for the fishery off the coast in the Northwest Mediterranean near
Blanes which forms the basis of the paper

> de Juan, S., Delius, G. & Maynou, F. *A model of size-spectrum
> dynamics to estimate the effects of improving fisheries selectivity
> and reducing discards in Mediterranean mixed demersal fisheries.*
> Fisheries Research 266, 106764 (2023)
> <https://doi.org/10.1016/j.fishres.2023.106764>

## Installation

You can install the mizerShelf package from GitHub with

``` r
remotes::install_github("gustavdelius/mizerShelf")
```

You may be prompted to update some of your existing packages. The two
packages that you should always update are `mizer` and
`mizerExperimental`, because the mizerShelf package will always be
designed to work with the most recent version of these.
