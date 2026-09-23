# okay-frege

Frege (a Haskell for the JVM) programs as okay programs: `Prog`, a thin Frege monad whose operations are okay's (`await`, `tell`, `perform`) and whose `liftIO` takes existing Frege IO, walked by an okay driver — multi-shot handlers work, no threads. JVM; Frege 3.25.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-frege.md`](../docs/modules/okay-frege.md) | what it is, and the reasoning |
| [`specs/frege.md`](../specs/frege.md) | the design and its decisions |
