## okay2-refine-route - refine-route on the Scala 2 core

okay2-refine gains the `Router`: `route[X]` by class (a `ClassTag` —
exact in Scala 2, which has no unions), `route { case … }` by pattern
(several kinds into one stream as pattern alternatives), `byName`,
`routeAs`, `tap`, `otherwise(Rejected)`, `run(source)` closing every
channel once (failing them on an input failure), `decide`, and
`Refine.routed` as the synchronous Stage. TestRouter (8 tests) is
JVM-only (`src/test/scala-jvm`: it blocks on the scheduler);
okay2-platform is a test dependency. docs/okay2.md §31 extended.
