## pwc-kafka-source - KafkaChunks as Source[Chunk[ConsumerRecord]]

The last module on the G-effectful `Produce` carrier: okay-kafka's
`KafkaChunks[K, V]` was `Chunk[ConsumerRecord[K, V]] ! (Produce + Async)`
and is `Source[Chunk[ConsumerRecord[K, V]]]` — one poll, one told
chunk, the thread parked in `poll` between them exactly as before;
`source` never ends, so nothing changed at an end. The test's
hand-rolled `firstChunk` (a `<|>` split with a cast) is
`Writer.uncons(...).map(Right => chunk)`. docs/modules/okay-kafka.md's
three mentions of the old shape updated.

With this, no module outside the core and okay-stream's own bridges
builds a `Produce` stream: what remains is `Producer` itself
(Generate.scala — `produce`, `Put[Producer]`, `Producer.fold/each/
concat/log`, its two `Stream` instances, `Foldable[Producer]`) and
`Source.fromProducer/ofProducer/toProducer`, plus the core tests and
one okay-cats interop written against them — the spec's "deletions
land with the last module" bullet, which is a decision for the
operator (delete `Producer`, or keep it as the documented pure special
case), not this lane's.

Files: okay-kafka KafkaInterop.scala, TestKafkaInterop.scala,
docs/modules/okay-kafka.md, the arc's spec and sprint checklists.
