# Carrion dynamics

Calculates the carrion biomass at the next timestep from the current
carrion biomass.

## Usage

``` r
carrion_dynamics(params, n, n_other, rates, dt, ...)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
  object

- n:

  A matrix of current species abundances (species x size)

- n_other:

  Other dynamic components. Only `n_other$carrion` is used.

- rates:

  A list of rates as returned by
  [`getRates()`](https://sizespectrum.org/mizer/reference/getRates.html)

- dt:

  Time step size

- ...:

  Unused

## Value

A single number giving the carrion biomass at next time step

## Details

The time evolution of the carrion biomass \\B\\ is described by
\$\$dB/dt = \tt{production} - \tt{consumption} \* B\$\$ where

- `consumption` is the mass-specific rate of consumption due to
  consumption calculated with
  [`carrion_consumption_ms()`](https://sizespectrum.org/mizerShelf/reference/carrion_consumption_ms.md),

- `production` is the rate at which the rest of the system produces
  carrion biomass, calculated with
  [`getCarrionProduction()`](https://sizespectrum.org/mizerShelf/reference/getCarrionProduction.md).

The dynamical equation is solved analytically to \$\$B(t+dt) =
B(t)\exp(-\tt{consumption} \cdot dt)
+\frac{\tt{production}}{\tt{consumption}} (1-\exp(-\tt{consumption}
\cdot dt)).\$\$ This avoids the stability problems that would arise if
we used the Euler method to solve the equation numerically.

## See also

[`detritus_dynamics()`](https://sizespectrum.org/mizerShelf/reference/detritus_dynamics.md)
