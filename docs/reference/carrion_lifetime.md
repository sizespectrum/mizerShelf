# Expected carrion lifetime

The expected carrion lifetime is defined as the inverse of the
mass-specific carrion consumption rate.

## Usage

``` r
carrion_lifetime(params)

carrion_lifetime(params) <- value
```

## Arguments

- params:

  A MizerParams object

- value:

  A number with the new value for the expected lifetime in years.

  Assigning a new value to the carrion lifetime rescales the carrion
  abundance while keeping the total consumption and decomposition of
  carrion the same (by adjusting the interaction strength of species
  with carrion and the mass-specific decomposition rate).

## Value

The number giving the expected lifetime in years.
