# mizerShelf marker classes

S4 marker subclasses of
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
and [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim.html)
that enable S3 dispatch for shelf-specific methods such as
[`steady()`](https://sizespectrum.org/mizerShelf/reference/steady.md),
[`scaleModel()`](https://sizespectrum.org/mizerShelf/reference/scaleModel.md),
[`removeSpecies()`](https://sizespectrum.org/mizerShelf/reference/removeSpecies.md),
[`addSpecies()`](https://sizespectrum.org/mizerShelf/reference/addSpecies.md),
and
[`getBiomass()`](https://sizespectrum.org/mizerShelf/reference/getBiomass.md).

## Details

Objects of class `mizerShelf` are created by
[`newDetritusCarrionParams()`](https://sizespectrum.org/mizerShelf/reference/newDetritusCarrionParams.md).
Objects of class `mizerShelfSim` are returned automatically by
[`project()`](https://sizespectrum.org/mizer/reference/project.html)
when called on a `mizerShelf` params object.
