## distinct-on-handlers - the Scala 3 handlers refuse a row they would misroute, and Writer's splits test the Writer first

- Operator: what okay2 guards, the Scala 3 core must guard too.
  `Distinct[R]` was required only by `Handler.union`/`flat`, so
  `Reader.run(7)` over `Reader % Int + Reader % String` compiled and
  failed at run time (TestRowIdentity asserted the ClassCastException).
  Now every handler that splits a PARAMETERISED signature out of an
  open row requires it: State `handle`/`zoomWith`, Reader
  `run`/`unlift`/`local`, Writer `run`/`collect`/`fold`/`foldWith`/
  `foldUntil`/`map`/`expand`, `runEither`/`runThrows`/`runUnsafe`/
  `orElse`, and `!.relay`/`translate`/`interpret` (`interpret` checks its
  target row too). TestRowIdentity keeps its demonstration under
  `Distinct.unchecked()`; TestDistinct pins the refusals.
- `Distinct.unchecked` returns one shared witness instead of allocating:
  the handlers ask for one per call, in measured loops.
- FOUND ON THE WAY, a silent wrong answer: with the rest left to
  inference inside an enclosing handler, `Writer.map(p)(f)` solves its
  rest as `Writer % W` itself (`F | F` is `F`, which Distinct cannot
  see), and `map` tested the REST first — every Say was forwarded
  unmapped (`Vector(1)` for `Vector(2)`, measured). `Writer.uncons` did
  the same and answered "no elements". The Distinct refusal caught it in
  `ProducerWriterCarrierBenchmark.chunksMapWriter`, which had been
  folding undoubled chunks; its rest is named now, and any number
  recorded for that lane before today measured the wrong program.
  `map`, `expand`, `uncons`, the stream iterator, `Source.toProducer`
  and `Pipe`'s two pulls test the Writer first now (`widen` does not
  need to: forwarding a Say is re-telling it). The two `Pipe.through`
  overloads lost a `TypeableK[G]` bound nothing used any more.
- Callers that filled `Writer.fold`/`foldUntil`'s using clause
  positionally (okay-blob, the facade's Services, eight benchmarks, two
  tests) pass the new clause first. docs/many-instances.md says where
  the check is asked and the self-rest shape.
