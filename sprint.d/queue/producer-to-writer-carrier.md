- [ ] **producer-to-writer-carrier** — move the library's pure pull
      stream from `Producer[A] = A ! Produce` (element = answer, identity
      signature) onto the writer carrier `Unit ! Writer % W` / `Source[W]`
      (element named in the type), MEASURED FIRST. Spec:
      specs/producer-to-writer-carrier.md — read it whole before picking.

      STAGE 0 IS DONE (2026-09-19, `compare/src/jmh/scala/okay/
      ProducerWriterCarrierBenchmark.scala`, 3 rounds, host load 48.7 (busy,
      a sibling's full `sbt test`) then 2.7 (quiet) — round 3 is the one
      to trust for magnitude, every sign held across all three). VERDICT,
      MIXED and diagnosed, full table in the spec's `## Results`:
        - elementwise streaming (both Async-shaped and pure) and chunked
          MAP+fold: the writer carrier WINS, 14-24% faster, 4 independent
          measurements, all same-signed 3/3 rounds. Two of them confirm
          12120c2a's Say-node number on a fresh benchmark.
        - chunked FOLD specifically: the writer carrier LOSES ~2x
          (5.175 vs 2.527 us, N=10000/64, same sign 3/3). CAUSE: `Chunks.
          foldLeft` dispatches `Fold.OfLong` once outside the loop
          (unboxed long throughout); `Writer.fold` dispatches on the TOLD
          type (`Chunk[Long]`, never `Long`), so it always takes the
          generic case, boxing the accumulator once per CHUNK.
        - the bridges (`ofProducer`/`toProducer`) tax 75-90% over a direct
          fold, as expected — the number stage 2 deletion recovers.
        - the blob byte lane shows the SAME tax as the chunked-fold loss,
          same direction, too small to act on (3-6%, 64 chunks not 157,
          `Unit` accumulator not boxed).
      6 rows in src/jmh/history.tsv, sha `producer-writer-carrier-stage0`.

      DECISION: stage 1 proceeds — the loss is confined to `Chunks.fold`,
      not the carrier (map and every elementwise shape clear the bar),
      which is exactly the spec's own "loses only on Chunks" condition
      for stage 1. Stage 2's `Chunks` migration gets ONE NEW PREREQUISITE
      the spec did not have before today: a chunk-aware specialized fold
      for the writer carrier (the analogue of `Chunks.foldLeft`/
      `Fold.OfLong`'s dispatch, inlining the ELEMENT's `Fold` into a
      per-chunk loop instead of `Writer.fold`'s generic per-chunk box),
      measured at parity with `Chunks.fold`, BEFORE `Chunks[A]` retypes.
      Until it exists, `Chunks` stays on `Producer` — a legitimate,
      narrow, documented exception, not a reason to stop the rest.

      NEXT CLAIMABLE SLICE = STAGE 1: docs/guide.md states the rule (new
      seams name their element in the type — `Source[W]` under Async,
      the pure writer stream otherwise), a `compileErrors` test that
      `pure(a)` is not an element on that carrier, and the pure writer
      stream gets a name (an operator naming call — ASK, do not invent;
      candidates and the "no third carrier name" constraint are in the
      spec's Decisions).
      THEN stage 2 per module, leaves first (okay-blob, okay-cluster,
      okay-persist, okay-sql/okay-jdbc, okay-docs, the kafka/fs2/zio/java
      interops), `Chunks` last and gated on the fold combinator above;
      each lane `sbt Test/compile` repo-wide before its gate; deletions
      (`produced`, `Producer.fold/each/concat`, the Produce Stream
      instances, the three Source bridges) land with the last module.

      GOTCHAS: `put-de-diagonal` (claim 307304cb) edits Generate.scala —
      check `.work/active/` and `git worktree list` before touching that
      file, not just the room (a silent sibling is normal here,
      [[okay-claim-discipline]] 2026-09-19 addendum); its `Put[Producer]`
      instance follows Producer's fate. Stage 0 ALSO found and fixed a
      pre-existing, unrelated build break: `compare`'s `dependsOn` never
      picked up `okayData`/`okayStm` after core-modularise moved `Sketch`
      and `Stm`/`Tx`/`TRef` out of `okay`, so `compare/Jmh/compile` was
      broken on master for anyone touching that project (fixed in
      build.sbt, landed with this stage). ~60 main files name
      Producer/produce/Chunks (okay-stream, blob, cluster, persist,
      sql/jdbc, docs, kafka/fs2/zio/java interops) — count with grep
      before each module lane, not from this line.

      DONE-WHEN (whole arc): no `Produce` in main sources except where
      `Chunks` is a documented exception (or the fold combinator above
      closes even that), `Blob.get` typed `Either[String, Unit] !
      (Writer % Chunk[Byte] + Async)`, docs/benchmarks.md rows
      re-measured for the shapes that changed, spec Results filled per
      stage.
