## chunks-fold-2x-gap-abstract-probe - the last named hypothesis, measured and refuted

The backlog entry `chunks-fold-vs-foldleft-2x-gap` asked why the
library `Chunks.fold` over a Producer of chunks read 2.53 us while
every Producer loop compiled in `compare` read 4.5-4.8, and named its
next probe: the same loop with `A` kept abstract at the loop, since
with `A = Long` dotty unboxes `c(i)` before `addLong` boxes it again —
the one semantic difference found. Built as
`Probe.foldProducerAbstract`/`foldFeedAbstract` (a generic method
dispatching on `Fold.OfLong[A]`, the library arm's exact shape) and
measured beside the concrete rows, JDK 21.0.12, N=10000/64: Producer
4.76 vs 4.61, Feed 2.60 vs 2.61, allocation identical. Refuted, like
the placement hypothesis before it.

The entry moves to refuted-declined-or-answered: after the `Chunks`
retype the fast row no longer exists in the library, so nothing is
priced by the gap, and only `-prof perfasm` — absent here — could go
further. The probe rows stay in the benchmark as the record.

Files: compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala,
src/jmh/history.tsv, backlog.d (moved).
