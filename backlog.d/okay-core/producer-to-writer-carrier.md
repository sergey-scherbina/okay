- [ ] producer-to-writer-carrier — `Producer[A] = A ! Produce`,
      `Produce = Id` (Generate.scala): the operation IS the element, so
      the element type is the ANSWER type and nothing else tells them
      apart. Two consequences, both already paid for. (1) `pure(a)`
      type-checks wherever `produce(a)` does and emits NOTHING — the
      producer's `Pure` is its end — measured the hard way in
      okay-watch (a zero-byte object under the right key,
      blob-source-seam; the warning now lives on `produce`'s scaladoc,
      which is a comment, not a type). (2) A row `Produce + G` can only
      be split by testing G, because a produced `String` is just a
      `String` at run time — the exact caveat `Writer` carried under
      its identity encoding until 12120c2a made it a one-constructor
      GADT (`Say(w)`), measured at NO cost on the real WriterBenchmark
      (198.0 against 203.2 us) after five attempts to keep the identity
      representation failed. Producer still carries that caveat, and
      `produced[A](e: Any): A` is its cast. The writer carrier
      `Unit ! Writer % W` (async: `Source[W]`) is the same stream with
      the element in the type and the answer separate — and
      `Producer.each` keeping "the producer's answer" for `Blob.get`'s
      outcome becomes simply `Either[String, Unit] ! Writer % Chunk[Byte]
      + Async`. The API is DUPLICATED today: `Producer.{fold, each,
      concat, log}` beside `Writer.{fold, uncons, of}`, two Stream
      instances each. Footprint (2026-09-19): `Producer`/`produce`/
      `Chunks` in ~60 main files — okay-stream (Chunks, Source, Bulk,
      Pipeline, ParallelChunks, Tables), okay-blob, okay-cluster
      Flow/Flows, okay-jdbc, okay-docs, okay-persist, okay-sql, kafka/
      fs2/zio/java interops. STAGED, each stage its own lane:
      (0) MEASURE before deciding — A/B `Producer[Chunk[A]]` against
      `Unit ! Writer % Chunk[A]` on the shapes Producer is hot in:
      `Chunks.fold`/`map` loop, `Stream.fold` per element, the
      `Source.fromProducer`/`toProducer` bridges, one Blob byte lane;
      alternate arms, count beside time (performance skill). 12120c2a's
      number is for Writer's own loop, not for Chunks' — do not reuse
      it. Decision gate: within noise -> stage 1; a real loss ->
      record it here and stop, Producer stays for the chunked hot path
      and the rule below still applies to new seams.
      (1) RULE + alias: new streaming seams are typed on the writer
      carrier; `Producer` becomes the documented pure special case
      (or an alias, if stage 0 shows the carriers can be one). Close
      the trap: with the element in the type, `pure(a)` no longer
      type-checks as an element.
      (2) MIGRATE by module, `Chunks[A]` retyped last (it is the hot
      one); delete `produced`, `Producer.{fold,each,concat}` in favour
      of Writer's, the second Stream instance pair, and the
      `Source.fromProducer`/`toProducer` bridges that exist only
      because the two carriers differ. Each module lane: Test/compile
      first across the repo (signature change), then the gate.
      Pairs with [[put-de-diagonal]] (independent; that one can land
      first and keeps a Producer instance until this decides).
