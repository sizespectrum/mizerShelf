# Remove some species from a shelf model

Extends
[`mizer::removeSpecies()`](https://sizespectrum.org/mizer/reference/removeSpecies.html)
for `mizerShelf` objects: after removing the species it also removes the
corresponding rows from the carrion encounter rate matrix `rho`.

## Usage

``` r
# S3 method for class 'mizerShelf'
removeSpecies(params, species, ...)
```

## Arguments

- params:

  A `mizerShelf` params object.

- species:

  The species to be removed. A vector of species names, or a numeric
  vector of species indices, or a logical vector indicating for each
  species whether it is to be removed (TRUE) or not.

- ...:

  Passed to
  [`mizer::removeSpecies()`](https://sizespectrum.org/mizer/reference/removeSpecies.html).

## Value

A `mizerShelf` params object with fewer species.

## Examples

``` r
params <- NWMed_params
species_params(params)$species
#>  [1] "Small DF worms"              "Small DF crustacea"         
#>  [3] "DF worms"                    "Endobenthic pred. crustacea"
#>  [5] "Suprabenthic crustacea"      "Endobenthic pred. worms"    
#>  [7] "Large DF worms"              "Starfish"                   
#>  [9] "Nut clam"                    "Murex"                      
#> [11] "Angular crab"                "Harbour crab"               
#> [13] "Red snapping shrimp"         "Spotted flounder"           
#> [15] "Black goby"                  "Gurnards"                   
#> [17] "Red mullet"                  "Striped red mullet"         
#> [19] "Hake"                        "Angler fish"                
#> [21] "Poor cod"                    "Horse mackerel"             
#> [23] "Shortfin squid"              "Blue whiting"               
#> [25] "Horned octopus"             
params <- removeSpecies(params, c("Poor cod", "Horse mackerel"))
species_params(params)$species
#>  [1] "Small DF worms"              "Small DF crustacea"         
#>  [3] "DF worms"                    "Endobenthic pred. crustacea"
#>  [5] "Suprabenthic crustacea"      "Endobenthic pred. worms"    
#>  [7] "Large DF worms"              "Starfish"                   
#>  [9] "Nut clam"                    "Murex"                      
#> [11] "Angular crab"                "Harbour crab"               
#> [13] "Red snapping shrimp"         "Spotted flounder"           
#> [15] "Black goby"                  "Gurnards"                   
#> [17] "Red mullet"                  "Striped red mullet"         
#> [19] "Hake"                        "Angler fish"                
#> [21] "Shortfin squid"              "Blue whiting"               
#> [23] "Horned octopus"             
```
