# Get biomass of species and components for a shelf model

Extends
[`mizer::getBiomass()`](https://sizespectrum.org/mizer/reference/getBiomass.html)
for `mizerShelf` and `mizerShelfSim` objects by adding the detritus
(resource spectrum) and any scalar other-component biomasses (e.g.
carrion).

## Usage

``` r
# S3 method for class 'mizerShelf'
getBiomass(object, ...)
```

## Arguments

- object:

  A `mizerShelf` or `mizerShelfSim` object.

- ...:

  Passed to the base
  [`mizer::getBiomass()`](https://sizespectrum.org/mizer/reference/getBiomass.html)
  method.

## Value

For `mizerShelf`: a named numeric vector of species/component biomasses.
For `mizerShelfSim`: an `ArraySpeciesByTime` matrix (time x
species/component) with species biomasses followed by Detritus and other
component biomasses.

## Examples

``` r
getBiomass(NWMed_params)
#>              Small DF worms          Small DF crustacea 
#>                6.514945e-01                1.603410e-01 
#>                    DF worms Endobenthic pred. crustacea 
#>                7.829609e-01                7.135015e-02 
#>      Suprabenthic crustacea     Endobenthic pred. worms 
#>                3.067995e-02                3.784602e-01 
#>              Large DF worms                    Starfish 
#>                6.473529e+00                6.038226e-02 
#>                    Nut clam                       Murex 
#>                3.124139e-03                1.243300e-01 
#>                Angular crab                Harbour crab 
#>                1.359802e-01                7.137132e-02 
#>         Red snapping shrimp            Spotted flounder 
#>                6.862409e-02                1.833424e-01 
#>                  Black goby                    Gurnards 
#>                6.904834e-02                1.438723e-01 
#>                  Red mullet          Striped red mullet 
#>                6.077036e-03                3.068191e-03 
#>                        Hake                 Angler fish 
#>                1.497468e-02                4.956061e-03 
#>                    Poor cod              Horse mackerel 
#>                5.001440e-03                1.280838e-02 
#>              Shortfin squid                Blue whiting 
#>                8.499166e-03                9.740222e-03 
#>              Horned octopus                    Detritus 
#>                1.544716e-02                2.544998e+02 
#>                     carrion 
#>                4.650807e-02 
# \donttest{
sim <- project(NWMed_params, t_max = 3)
getBiomass(sim)
#> Biomass (4 times x 27 species) [g] 
#>   Small DF worms: min=0.651 mean=0.652 max=0.652
#>   Small DF crustacea: min=0.16 mean=0.161 max=0.161
#>   DF worms: min=0.783 mean=0.783 max=0.783
#>   Endobenthic pred. crustacea: min=0.0714 mean=0.0714 max=0.0714
#>   Suprabenthic crustacea: min=0.0307 mean=0.0307 max=0.0307
#>   Endobenthic pred. worms: min=0.378 mean=0.378 max=0.378
#>   Large DF worms: min=6.47 mean=6.47 max=6.47
#>   Starfish: min=0.0604 mean=0.0604 max=0.0604
#>   Nut clam: min=0.00312 mean=0.00312 max=0.00312
#>   Murex: min=0.124 mean=0.124 max=0.124
#>   Angular crab: min=0.136 mean=0.136 max=0.136
#>   Harbour crab: min=0.0714 mean=0.0714 max=0.0714
#>   Red snapping shrimp: min=0.0686 mean=0.0686 max=0.0686
#>   Spotted flounder: min=0.183 mean=0.183 max=0.183
#>   Black goby: min=0.069 mean=0.069 max=0.069
#>   Gurnards: min=0.144 mean=0.144 max=0.144
#>   Red mullet: min=0.00608 mean=0.00608 max=0.00608
#>   Striped red mullet: min=0.00307 mean=0.00307 max=0.00307
#>   Hake: min=0.015 mean=0.015 max=0.015
#>   Angler fish: min=0.00496 mean=0.00496 max=0.00496
#>   Poor cod: min=0.005 mean=0.00501 max=0.00501
#>   Horse mackerel: min=0.0128 mean=0.0128 max=0.0128
#>   Shortfin squid: min=0.0085 mean=0.0085 max=0.0085
#>   Blue whiting: min=0.00974 mean=0.00974 max=0.00974
#>   Horned octopus: min=0.0154 mean=0.0154 max=0.0154
#>   Detritus: min=254 mean=255 max=255
#>   carrion: min=0.0465 mean=0.0465 max=0.0465
# }
```
