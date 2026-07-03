# Detritus dynamics

Calculates the detritus size spectrum at the next time step from the
current size spectrum. The size spectrum is always held at a power law
with the same exponent – only the intercept is dynamical to reflect the
change in total detritus biomass.

## Usage

``` r
detritus_dynamics(params, n, n_pp, n_other, rates, dt, ...)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
  object

- n:

  A matrix of current species abundances (species x size)

- n_pp:

  Vector of detritus density

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

A vector giving the detritus spectrum at the next time step.

## Details

The time evolution of the detritus biomass \\B\\ is described by
\$\$dB/dt = \tt{production} - \tt{consumption} \* B + \tt{external}\$\$
where

- `consumption` is the mass-specific rate of consumption.

- `production` is the rate at which the rest of the system produces
  detritus biomass.

The dynamical equation is solved analytically to \$\$B(t+dt) =
B(t)\exp(-\tt{consumption} \cdot dt)
+\frac{\tt{production}}{\tt{consumption}} (1-\exp(-\tt{consumption}
\cdot dt)).\$\$ This avoids the stability problems that would arise if
we used the Euler method to solve the equation numerically.

## See also

[`carrion_dynamics()`](https://sizespectrum.org/mizerShelf/reference/carrion_dynamics.md)
