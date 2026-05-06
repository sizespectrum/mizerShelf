# Scale model parameters

Extends
[`mizer::scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html)
for `mizerShelf` objects: adjusts the carrion encounter rates and the
external detritus input before delegating to the base method.

## Usage

``` r
# S3 method for class 'mizerShelf'
scaleModel(params, factor, ...)
```

## Arguments

- params:

  A `mizerShelf` params object.

- factor:

  A numeric value by which to scale the model.

- ...:

  Passed to
  [`mizer::scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html).

## Value

A `mizerShelf` params object with scaled parameters.

## Examples

``` r
params <- scaleModel(NWMed_params, 0.5)
```
