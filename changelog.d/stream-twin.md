## streams: one front, two backends

Lane stream-twin. `Streaming[S]` is the stream's interface and its words
(`map`, `filter`, `take`, `++`, `flatMap`, `evalMap`, `foldLeft`, `toVector`,
`merge`), with two backends chosen by import: `okay.streams.classic` — the
classic `Source` (a Writer push) behind an opaque `Flow` — and
`okay.streams.machine` — `StreamCont`, a pull over the machine's Async, with
bridges to and from `Source`. One test suite runs on both. The classic `Source`
gains stream-level `filter`/`take`/`flatMap`/`evalMap`/`foldLeft` through the
front. Measured: the machine's pull is 2.1x the classic on a bare range, 1.19x
mapped (StreamContBenchmark).
