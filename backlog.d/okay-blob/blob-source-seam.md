- [x] blob-source-seam — DONE 2026-09-16 (blob-source-road), ADDITIVELY:
      `Source.fromProducer`/`ofProducer`/`toProducer` in core,
      `putSource`/`getSource` concrete on the trait, the primitives
      untouched, no engine changed. A test asserts the asymmetry that
      justified it: `pure(x)` a silent nothing at Produce, a type error
      at Source. Re-typing the primitives themselves stays declined
      until something needs `Flush.now` at the engine. Was: the real fix, and the expensive one: re-type
      the seam on `Source[Chunk[Byte]]` (`Unit ! (Writer % W +
      Async)`) instead of `Chunk[Byte] ! (Produce + Async)`. The
      answer becomes `Unit` and the element type moves into the
      SIGNATURE, so `pure(x)` can no longer be mistaken for an emit —
      the defect stops being expressible rather than being documented.
      It also buys `Source.of`, `Source.unfold` and `Source.apply` as
      ready constructors, every Writer/Stream combinator, `merge`, and
      `Flush.now` — an explicit chunk boundary, which is precisely
      what `S3.put`'s own comment wants when streaming bodies and
      multipart arrive. COST, stated: `Blob.put`, `Blob.get`,
      `Blob.list`, `Blob.Counted`, `Fs`, `S3` and `Backup` all move,
      and `get`'s `Either[String, Unit] ! (Produce + Async)` needs a
      shape that carries an outcome beside a Source. Worth pricing
      before taking, and worth taking only if the seam is going to be
      touched for streaming puts anyway.
