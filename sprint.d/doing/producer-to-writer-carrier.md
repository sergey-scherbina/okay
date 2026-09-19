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

      THE CHUNK-AWARE FOLD PREREQUISITE, HALF DONE (2026-09-19). Built
      `Chunks.foldLeftWriter`/`foldWriter` (okay-stream/src/main/scala/
      Chunks.scala, on `Writer.foldWith`). MIXED, 3-round-measured, full
      writeup in the spec's `## Results`: `foldLeftWriter` (literal
      step) reaches PARITY with `Chunks.foldLeft` (5.00 vs 4.76 us/op)
      — this is the shape okay-cluster's `Flows.scala`/`Job.scala` call
      sites already use, so THOSE can migrate now. `foldWriter` (the
      `Fold`-instance-dispatched form `Chunks.fold`/`agg.fold` need —
      `Bulk.scala`/`Pipeline.scala`/`Acceptance.scala`, the only 3 call
      sites) does NOT reach parity — 17.30 us/op, 3.5x the direct call,
      6.8x `Chunks.fold` itself, stable across three tried
      implementations. Diagnosed as far as this environment allows (no
      profiler available); root cause and what's ruled out are in
      Chunks.scala's doc comments on both combinators — read them before
      a retry so it doesn't repeat the same three shapes. `writerk-
      companion-scope` landed alongside this (found while writing this
      combinator's tests): `given writerK` moved into `object Writer`'s
      own body so it resolves from any package with no import — a bare
      top-level given wasn't found without one, forcing `import
      okay.writerK` + `@nowarn` at every `Writer.fold`/`.collect` call
      site on a parameterized W; now just the `@nowarn` remains (E092 is
      a real, sound-by-construction erasure caveat, not fixable the same
      way).

      STAGE 1 IS DONE (2026-09-19). The pure writer stream is named
      `Feed[W] = Unit ! Writer % W` (src/main/scala/Writer.scala,
      operator's choice over no-alias and `Told[W]`). docs/guide.md
      states the rule: a new streaming seam names its element in the
      type — `Feed[W]` when it performs no other effect, `Source[W]`
      when it performs `Async`; `Producer` is the pure special case for
      code already written against it. ONE CORRECTION FROM THE ORIGINAL
      PLAN, found while writing the test: the `pure(a)` trap is NOT
      closed by a type error at `Feed`/`Source` — `compileErrors` proved
      the wrong tool to test it (it reports hard errors only; munit's
      macro drops warnings entirely, verified directly against the
      classic `val u: Unit = 5` shape before trusting it). The real
      mechanism, checked by an actual `sbt` compile: `val f: Feed[Int]
      = pure(5)` prints `[E190] ... Discarded non-Unit value`, which
      this repo's gate refuses as any other warning — inspectable, not
      type-closed. `TestGenerate` documents this and asserts what IS
      testable (no hard error, same as `Producer`); `TestSourceProducer`
      carried the same over-claim for `Source` in its own comment,
      unverified by its own test — left alone, not wrong in substance,
      just under-proven; not mine to touch.

      STAGE 2, MODULE 1/6 (okay-blob) IS DONE (2026-09-19). `Blob.get`/
      `put`/`list` all three retyped to the writer carrier;
      `getSource`/`putSource` deleted (redundant now); every
      `Producer.each`/`Producer.concat` call site in Fs/S3/Backup/Offload
      became `Writer.fold`/`Writer.collect`. `S3.get` simplified for
      real, not just renamed: its response body was ALREADY a
      `Source[Chunk[Byte]]` from okay-http, so the old code was paying
      exactly the bridge tax stage 0 measured (walking it chunk by chunk
      back into a `Produce` row); now it's `Writer.expand(src)(filter)
      .map(_ => Right(()))`, no bridge crossed. `E092`'s TypeableK
      caveat shows up at every `Writer.fold`/`Writer.collect` call site
      typed on a parameterized `W` (here `Chunk[Byte]`/`Chunk[Meta]`) —
      `@nowarn("msg=cannot be checked at runtime")` at each one, same as
      the compare-jmh benchmark from stage 0; if stage 2 keeps
      accumulating these, a scoped `-Wconf` entry might be worth
      proposing instead of one `@nowarn` per call site — not decided
      here. Full blast-radius check: okay-ops/okay-demo/okay-docs-dynamo/
      okay-acme depend on okay-blob but only for `Blob.Stats`/SigV4,
      untouched; okay-watch (Blob.get -> Producer.each restore) is a
      private repo, not reachable from here. Gate: okayBlobJVM/test
      21/21 green; all three platforms plus every dependent module
      cold-compiled, zero warnings throughout.

      NEXT CLAIMABLE SLICE = STAGE 2, MODULE 2/6: okay-cluster
      Flow/Flows/Job — `Chunks.foldLeft` call sites (6 in Flows.scala, 1
      in Job.scala) migrate onto `foldLeftWriter`, now at parity, per
      above. Then okay-persist Streams/Wire, okay-sql/okay-jdbc,
      okay-docs and its backends, the kafka/fs2/zio/java interops —
      leaves first, `Chunks` last and gated on `foldWriter` closing its
      remaining dispatch-tax gap (above); each lane `sbt Test/compile`
      repo-wide before its gate;
      deletions (`produced`, `Producer.fold/each/concat`, the Produce
      Stream instances, the three Source bridges) land with the last
      module. `writerK` (or `okay.given`) must be in scope at every
      `Writer.fold`/`.collect`/`.run` call site typed on a parameterized
      W — the E006 "Not Found" error names it if it's missing.

      GOTCHAS: `put-de-diagonal` LANDED as `c9a3f561` (2026-09-19) —
      `Generate.scala` is no longer a shared-edit hazard, and `Put[S[_]]`
      is now `def put[W](w: W): Unit /> S[W]` (undiagonalized, `Source`
      has an instance, `Teller` is gone); its `Put[Producer]` instance
      follows Producer's fate in stage 2. Stage 0 ALSO found and fixed a
      pre-existing, unrelated build break: `compare`'s `dependsOn` never
      picked up `okayData`/`okayStm` after core-modularise moved `Sketch`
      and `Stm`/`Tx`/`TRef` out of `okay`, so `compare/Jmh/compile` was
      broken on master for anyone touching that project (fixed in
      build.sbt, landed with this stage). ~60 main files named
      Producer/produce/Chunks before okay-blob's 6 files moved off —
      count with grep before each remaining module lane, not from this
      line: it is already stale.

      DONE-WHEN (whole arc): no `Produce` in main sources except where
      `Chunks` is a documented exception (or the fold combinator above
      closes even that), `Blob.get` typed `Either[String, Unit] !
      (Writer % Chunk[Byte] + Async)` — DONE, docs/benchmarks.md rows
      re-measured for the shapes that changed, spec Results filled per
      stage.
