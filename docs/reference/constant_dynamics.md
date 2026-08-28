# Keep a component's abundance constant

A dynamics function for use as `other_dynamics` for a component that is
held at its current value, i.e. it never changes over time.

## Usage

``` r
constant_dynamics(params, n_other, component, ...)
```

## Arguments

- params:

  A MizerParams object

- n_other:

  A list with the current biomasses/abundances of the other components

- component:

  The name of the component whose abundance is returned unchanged

- ...:

  Unused

## Value

The unchanged abundance of `component`
