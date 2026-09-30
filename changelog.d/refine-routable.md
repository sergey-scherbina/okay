## refine-routable - one routing table for every carrier: the Routable typeclass

okay-refine gains `Routable[C]` — the typeclass of what a `Routes` table
can be run over: `fan` (every input tagged once, a lane per index, the
rejects, the counts), `select`, `done`. Instances: `Vector`, any `Bulk`
(`Chunks`, `SparkBulk`'s rows), and a `Source` stream (a channel per lane,
`counts` as the driving program; `Routable.stream(capacity)` for bounded
lanes). `Routes.split(c)` takes any of them — the same table, the same
call — and replaces the Bulk-only split; the typeclass is on the carrier
value so aliases and opaque types resolve as written. TestRoutes: one
table over a Vector, Chunks and a Source agrees; a bounded stream loses
nothing; TestSparkRoutes unchanged in meaning.
