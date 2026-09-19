## feed-stream-given-name - the pure writer Stream instance is `feedStream`

`given [A]: Stream[[W] =>> A ! Writer % W, Pure]` (Writer.scala) was
anonymous while its G-effectful twin is `writerStreamIn`, so every
summon — four in Chunks.scala after the `Chunks` retype, three in the
compare JMH, one in TestGenerate — spelled the whole type lambda. It
is `feedStream` now, and a summon is `feedStream[Unit].iterator(p)`.
No behaviour change; the eight sites shortened.

Files: src/main/scala/Writer.scala, okay-stream/src/main/scala/
Chunks.scala, compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala,
src/test/scala/TestGenerate.scala.
