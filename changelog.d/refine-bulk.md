## refine-bulk - a routing table as a value, run on any Bulk: one JVM or Spark

okay-refine gains `Routes`: a table declared as an `object` whose lanes
are typed handles (`route[X]` exact for unions via `TypeTest`,
`route(name){ case … }`, `byName`, `routeAs`); `split(docs)` over any
`Bulk` recognises each document once, caches (lane, value) and answers
each lane as a typed collection, the rejects with why, and the counts in
one aggregate; `run(source)(lane ~> channel, rejected ~> channel)` binds
the same table to channels, rejecting an unbound lane's values rather
than dropping them. `Documents.files` (JVM) reads a directory as
(name, bytes), one split per file. `Refine` and `Merge` are
`Serializable`. TestRoutes (Chunks, channels, serialization round trips)
and TestSparkRoutes (400 documents on SparkBulk local[4] = one JVM);
okay-spark depends on okay-refine in test scope only.
