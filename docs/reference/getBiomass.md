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
For `mizerShelfSim`: an `ArrayTimeBySpecies` matrix (time x
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
#>     sp
#> time Small DF worms Small DF crustacea  DF worms Endobenthic pred. crustacea
#>    0      0.6514945          0.1603410 0.7829609                  0.07135015
#>    1      0.6514892          0.1605437 0.7829316                  0.07136983
#>    2      0.6515196          0.1606731 0.7829557                  0.07137636
#>    3      0.6515006          0.1607657 0.7829388                  0.07138660
#>     sp
#> time Suprabenthic crustacea Endobenthic pred. worms Large DF worms   Starfish
#>    0             0.03067995               0.3784602       6.473529 0.06038226
#>    1             0.03067721               0.3784432       6.473344 0.06039681
#>    2             0.03067501               0.3784540       6.473474 0.06040589
#>    3             0.03067453               0.3784561       6.473475 0.06041705
#> ... showing 8 of 27 species (Nut clam, Murex, Angular crab, Harbour crab, Red snapping shrimp, ...); use as.data.frame() for the full data. 
# }
```
