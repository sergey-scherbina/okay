## generators-jmh - `Gen` priced: the wrapper is free, `splice` is not

`compare/GenBenchmark` prices a `Gen` pipeline against the program it
wraps: 10 000 Longs unfolded on both roads, `toList`/`iterator`/
`map.filter`/`take` against `Writer.run`, `Writer.foldUntil` and
`Source.runCollect`, time and `-prof gc` bytes. Measured PER LANE,
gated on 20 s of instantaneous quiet (no sibling sbt or JMH fork, CPU
under 200%) and re-run when the box was busy at a lane's end — three
whole-matrix rounds on a box running sibling gates back to back read
±50–110% and were discarded on the record. Two findings. The value
class and the Stepper add no frame: `Gen.iterator` allocates 191 B/elem
against `Writer.run`'s 192, 150.5 vs 134.2 µs. `splice` does: `filter`
builds an `emit`/`empty` program per element, and `map.filter.toList`
reads 338.7 µs / 381 B/elem against 214.9 / 272 hand-written — +109 B
per element, filed as `gen-filter-as-walk` (backlog okay-core) with
the fix's shape (`taking`'s walk) and the row to re-measure. Between:
`Gen.read` is +40% over `Writer.foldUntil` on the same program (the
`Stop` arm in every split, the non-inline walk's closure); `take`
re-emits, +15%. Twelve rows in src/jmh/history.tsv (`gj-*`),
docs/benchmarks.md §21, specs/generators.md Results,
docs/direct-style.md "What it costs". Landed as ce3f5e8e.
