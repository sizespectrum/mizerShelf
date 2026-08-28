# mizerShelf marker classes

S4 marker subclasses of
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
and [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim.html)
that enable S3 dispatch for shelf-specific methods such as
[`tuneSteadyState()`](https://sizespectrum.org/mizerShelf/reference/tuneSteadyState.md),
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

The classes are **not** defined statically. Instead mizer creates them
when the package is loaded: `.onLoad()` calls
[`mizer::registerExtension()`](https://sizespectrum.org/mizer/reference/registerExtension.html),
which recognises mizerShelf as a dispatching extension from the S3
methods it registers for its marker class and inserts `mizerShelf` at
the correct place in the S4 hierarchy relative to any other extension
packages loaded in the same session. This lets mizerShelf be chained
with other extensions in either load order. A static
`contains = "MizerParams"` definition would fix mizerShelf as a direct
sibling of every other extension and prevent such chaining, because a
sealed class cannot be re-parented.
