- [ ] **producer-to-writer-carrier** — move the library's pure pull
      stream from `Producer[A] = A ! Produce` (element = answer, identity
      signature) onto the writer carrier `Unit ! Writer % W` / `Source[W]`
      (element named in the type), MEASURED FIRST. Spec:
      specs/producer-to-writer-carrier.md — read it whole before picking.
      WHY: `pure(a)` type-checks as an element and emits nothing (bit
      okay-watch, blob-source-seam); `Produce + G` splits only by G's
      runtime class; `produced` is a cast; `Producer.{fold,each,concat}`
      duplicate Writer's. Writer left the same representation in
      12120c2a at no measured cost (198.0 vs 203.2 us on WriterBenchmark)
      — but that is Writer's loop, not Chunks'.
      FIRST CLAIMABLE SLICE = STAGE 0, a benchmark-only lane: A/B
      `Producer[Chunk[A]]` vs `Unit ! Writer % Chunk[A]` on `Chunks.fold`
      and `Chunks.map` separately, elementwise `Stream.fold`, the
      `Source.fromProducer`/`toProducer` bridges, one okay-blob byte
      lane. Arms alternate in one invocation; print a count beside every
      time; host load in the row; append src/jmh/history.tsv; write the
      verdict with numbers into the spec's Results AND here. Existing
      shapes to extend rather than invent: src/jmh WriterBenchmark,
      ChunkBuildBenchmark, SourceChunkBenchmark; compare/
      FoldConsumersBenchmark. GATE OF THE DECISION: within noise (arms'
      difference < spread of two identical runs in the same session) ->
      stage 1; a real loss (same sign over three alternating rounds,
      above that spread) -> record it here, Producer stays for the
      chunked hot path, the rule for new seams applies anyway.
      THEN stage 1 (rule in docs/guide.md, `compileErrors` test that
      `pure(a)` is not an element, the pure writer stream named — an
      operator naming call, ask), then stage 2 per module, leaves first,
      `Chunks` last, each lane `sbt Test/compile` repo-wide before its
      gate; deletions (`produced`, Producer.fold/each/concat, the Produce
      Stream instances, the three Source bridges) with the last lane.
      GOTCHAS: Generate.scala is being edited by the put-de-diagonal lane
      (claim 307304cb) — rebase on it before touching that file; its
      `Put[Producer]` instance follows Producer's fate. ~60 main files
      name Producer/produce/Chunks (okay-stream, blob, cluster, persist,
      sql/jdbc, docs, kafka/fs2/zio/java interops) — count with grep
      before each module lane, not from this line.
      DONE-WHEN (whole arc): no `Produce` in main sources, `Blob.get`
      typed `Either[String, Unit] ! (Writer % Chunk[Byte] + Async)`,
      docs/benchmarks.md rows re-measured for the shapes that changed,
      spec Results filled per stage.
