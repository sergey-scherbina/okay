## scan-into-the-other-scanners (Markdown) - one scanner, as the entry asked

`Markdown.scan` (okay-codec) implemented `step` — the
`(S, Vector[Token[K]])` pair — and inherited `Scan`'s default
`stepInto`, which delegates to `step` and pays both the `Tuple2` per
character and the `Vector` per finished token (docs/benchmarks.md
§10 priced the combination at -29% for Json's own identical move).
It now extends `ScanInto` and writes `stepInto` directly onto the
`Growable` sink; `step` comes for free from the trait. `parse()`
already called `scan.stepInto` through `Scan`'s own interface, so no
caller changed anywhere.

Correctness: `TestCodec` and `TestLaws` pass unchanged, including the
ScalaCheck property "Markdown: the CST reproduces ANY input, exactly"
and "chunked lexing agrees with element-wise, at any chunk size".

Measured (a new `lexMarkdownElementwise` lane in
`compare/src/jmh/scala/okay/TextBenchmark.scala` — this backlog
entry's own ask, "a lane that measures one of them first"): min
20.362 μs/op after against 29.853 μs/op before, one clean alternating
round. A second round was too noisy to trust (load average climbed to
73 mid-session, several sibling agents gating concurrently at once)
and is not claimed as a number — only as not contradicting the
direction, which matches the mechanism Json's own conversion already
measured rather than a fresh guess.

One scanner, as the entry asked. `Yaml`, `Xml` and okay-rag's `Code`
scanner are unconverted; the backlog entry stays open, narrowed to
those three.
