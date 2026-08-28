# Controlling the carrion encounter rate in the tuning gadget

This control allows the user to adjust `rho_carrion`, the coefficient of
the species' encounter rate for carrion, for the selected species.

## Usage

``` r
rhoControl(input, output, session, params, params_old, flags, ...)

rhoControlUI(p, input)
```

## Arguments

- input:

  Reactive holding the inputs

- output:

  Reactive holding the outputs

- session:

  Shiny session

- params:

  Reactive value holding updated MizerParams object

- params_old:

  Reactive value holding non-updated MizerParams object

- flags:

  Environment holding flags to skip certain observers

- ...:

  Unused

- p:

  The MizerParams object currently being tuned.
