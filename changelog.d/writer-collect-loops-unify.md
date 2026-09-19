## writer-collect-loops-unify - three collect drains are calls to the one loop

`Writer.collect`, `Source.runCollect` and `Source.concat` were three
hand-written copies of `Writer.loopWith`'s walk, each accumulating
with `Vector :+` per element, and `concat` finishing with a `.map`
over a program still forwarding Async — the mapped-residual trap
`loopWith`'s own doc names. They were copies because a split on
`TypeableK[Writer % W]` was an unchecked E092 test at a parameterised
W; writer-typeablek-by-class (87be6174) made `writerK` the class of
`Say` on 2026-09-19, and the comments on all three still cited the
vanished reason.

`loopWith`'s finisher takes the answer too — `(S, A) => R` — so a
drain finishes INSIDE the loop; `foldWith` and `run` are the same
calls with a tuple, and the three drains are one line each: a cons
per element, one reverse (`concat` flattens the reversed list through
a sized `Vector` builder). `collect` no longer asks for a
`TypeableK[G]` it never used.

Measured (spec Results, history.tsv `wcl-*`, two rounds, each pair
beside its old shape written out verbatim as the control row):
`runCollect` on 10k elements 1 564 129 against 2 365 497 B/op (−34%,
the `:+` price either-scalarised measured on `Writer.run`) and 8–14%
time, same sign both rounds; `concat` on 64×1024 bytes at time PARITY
(the byte copy is the lane) and −4 KB / −8 KB B/op, the second on the
source with one `async` per chunk — 126 B per forwarded operation,
the mapped residual priced at a seam for the first time.

Filed from the same review (2026-09-20): `stream-fold-via-iterator`,
`direct-compileall-split` (backlog.d/okay-core), `fuse-plan-typed-term`
(backlog.d/optics-outside).

Files: src/main/scala/Writer.scala, okay-stream/src/main/scala/
Source.scala, compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala
(section 5, six rows; two stale E092 comments dropped),
specs/writer-collect-loops.md, src/jmh/history.tsv.
