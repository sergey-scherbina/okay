## okay2-refine-bulk - refine-bulk on the Scala 2 core

okay2-refine gains `Routes`: a routing table as an `object` with typed
lanes (`route[X]` by `ClassTag`, `route(name){ case … }`, `byName`,
`routeAs`), `split` over any okay2 `Bulk` (one recognition per document,
cached; lanes as filters; counts in one aggregate), `run` into channels
with `lane ~> channel` / `rejected ~> channel`; `Documents.files[D](dir)`
(JVM; okay2's `Bulk` has no `read(path, Format)`); `Refine` and `Merge`
Serializable. TestRoutes (scala-jvm, 4) and TestSparkRoutes (okay2-spark,
400 documents on local[4] = one JVM).
