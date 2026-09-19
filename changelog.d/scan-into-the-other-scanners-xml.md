## scan-into-the-other-scanners-xml - Xml.scan writes straight onto the sink

`Xml.scan` (the tag-nesting dialect) converted from `Scan` to
`ScanInto`, same shape as Markdown's earlier move: `stepInto` writes
finished tokens straight into the sink instead of building a
`(S, Vector[Token[K]])` pair, avoiding both the `Tuple2` per
character and the `Vector` per finished token
(docs/benchmarks.md §10). Mechanical for Xml specifically because
`step` never recurses into itself (unlike Yaml/Code, still open in
`scan-into-the-other-scanners`).

Measured (`compare/src/jmh/scala/okay/TextBenchmark.scala`,
`lexXmlElementwise`, new benchmark — tags, attributes, text, a
comment, CDATA, every branch of `stepInto`): 28.8 vs 39.3 us/op
before the conversion (same box, same run, `git stash` A/B on the
one file), all 8 `TestXml` cases still green.
