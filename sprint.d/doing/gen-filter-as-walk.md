- [~] gen-filter-as-walk — `Gen.filter` is `splice`, which builds an
      `emit`/`empty` program plus a `flatMap` PER ELEMENT; measured
      (generators-jmh, 2026-09-23, per-lane gated JMH, `-prof gc`):
      `Gen.unfold.map(_ * 2).filter(_ % 3 == 0).toList` over 10 000
      Longs reads 338.7 µs / 381 B/elem against 214.9 / 272 for
      `Writer.map` + a filtering fold — +109 B and +124 µs on 10k, the
      only pipeline combinator that is not parity with the hand road
      (`iterator` = `Writer.run` to the byte, `map` IS `Writer.map`).
      Write `filter` as a walk in the shape of `taking`/`takingWhile`
      (Gen.scala): re-tell on `p(w)`, skip to `k(())` otherwise, no
      program per element. Law: `TestGen`'s laziness counter unchanged
      (a filtered generator still runs the body no further than asked);
      number: the `gj-gc-gen-pipeline-vs-writer-pipeline10k` row
      re-measured toward 1.0. `withFilter`, `flatMap` over a
      single-element result and `zipWithIndex` are the same shape and
      could follow, each with its own row.
