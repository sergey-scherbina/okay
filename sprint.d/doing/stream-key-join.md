- [ ] stream-key-join — a join BY KEY over streams, not tables. Every
      join in the repository today holds one side whole: `Bulk.join`
      (okay-stream Bulk.scala) is the equi-join only, by contract
      (specs/bulk.md, "join is the equi-join only"), and its local
      instance is a hash join — the right side into a HashMap, the left
      side streamed; `Tables.join` is the same behind a plan node that
      turns the sides by estimated size; the Spark, Flink and
      `java.util.List` instances delegate to their platform. The demo's
      "stream join" (okay-demo Combine.scala) is a `merge` by readiness
      plus a hand-written enrichment `Stage`, not a join by key. None of
      them works on an unbounded `Source`, and nothing joins two
      `Chunks` without materialising one. THE ASK, in two stages, SPEC
      FIRST (specs/stream-join.md — the shape, what a key-ordered
      stream promises, what happens to unmatched rows): (1) SORT-MERGE
      join for streams already ORDERED by key — `Chunks.joinSorted(l, r)
      (using Ordering[K])` and the `Source` twin, one cursor per side
      advancing the smaller key, a run of equal keys on both sides
      producing the cross product of that run and nothing held beyond
      the current run; inner first, `left`/`full` as options answering
      `Option` on the missing side; this is [[source-zip]] that skips,
      so it lands after it and reuses its fiber-per-side shape and
      release law; (2) WINDOWED join for unbounded, unordered streams —
      EVENT-TIME, on the machinery specs/event-time-windows.md already
      landed (okay-stream `Windows`: `at: A => Long`, a bounded
      out-of-orderness watermark, `lateness`, dropped rows COUNTED):
      each side's row kept per key until the watermark passes its
      interval, a row matched against what the other side's window
      holds on arrival, never the machine's clock — so the test is a
      list, deterministic, like `Windows`' own; the Flink interval join
      and Kafka-Streams KStream-KStream join, which okay-flink today
      reaches only through `Bulk` for the bounded case.
      Refuted in advance: a hash join on `Source` — it is `Bulk.join`
      with a fiber, and the demo shows `Stage` does the enrichment
      shape already. Literature for the docs page (operator rule, docs
      ship with the lane): symmetric hash join (Wilschut & Apers 1991),
      Kafka Streams KStream-KStream windowed join, Flink interval join.
      Tests: agreement with `Bulk.join` on a bounded sorted input
      (same multiset of pairs), the run cross product, the unmatched
      side per variant, the window's eviction as the watermark moves
      and the late row counted, not silently joined.
      (2026-09-28, operator ask)
