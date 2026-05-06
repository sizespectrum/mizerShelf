# Contribution of unstructured components to the encounter rate

The encounter rate \\E_i(w)\\ for an unstructured resource like for
example carrion is proportional to the total biomass \\B\\ with a
coefficient \\\rhi_i(w)\\ that depends on the predator species \\i\\ and
the size of the predator: \$\$E_i(w) = \rho_i(w) B.\$\$

## Usage

``` r
encounter_contribution(params, n_other, component, ...)
```

## Arguments

- params:

  MizerParams

- n_other:

  Biomasses of unstructured components

- component:

  Name of component whose contribution is requested

- ...:

  Unused

## Value

Array (species x size) with the encounter rate in g/year.

## Details

The coefficient \\\rhi_i(w)\\ is stored as a matrix (species x size) in
the `rho` parameter of the component. It has units 1/year.
