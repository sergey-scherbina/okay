## okay2-refine-routable - the Routable typeclass on the Scala 2 core

okay2-refine gains `Routable[C]` (`fan`, `select`, `done`, with `Aux`
carrying the lane and count shapes) and instances for `Vector`, any
`Bulk` (Chunks and okay2-spark's `Rows`) and `Source` (a channel per lane,
`counts` as the driver; `stream(capacity)` for bounded lanes);
`Routes.split` takes any of them. Scala 2 unifies `Chunks[A]` through its
alias with `D[A]`, so the Scala 3 core's separate `Chunks` instance would
be ambiguous here and is absent. TestRoutes (one table over a Vector,
Chunks and a Source agrees; a bounded stream loses nothing),
TestSparkRoutes on the typeclass; docs/okay2.md §31.
