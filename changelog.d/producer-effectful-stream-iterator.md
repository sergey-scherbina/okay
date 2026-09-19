## producer-effectful-stream-iterator - the G-effectful producer walks without a program per step

`Stream[[A] =>> A ! Produce + G, G]` (Generate.scala) had no
`iterator` override, so a G-effectful producer walked through the
default `Iterator.unfold(s)(uncons(_).runWith)` — an `Option`, a
`Some`, and a `Free` node built and run per element. It has the
override now, the mirror of `writerStreamIn`'s (Writer.scala) with
`produced[A](e)` where that one matches `Say`: a mutable-state walk
that answers a forwarded G-operation with `Handler[G].handle` and
yields produced values directly.

**Measured, 2 rounds, JDK 21.0.12, N=10000 Async-widened produces,
both arms in one run:** 57.3 us/op against the default walk's 103.6
(written out verbatim as the control row), 876,944 against 1,436,960
B/op — 1.8x faster, 39% less garbage, 56 bytes per element gone;
`Stream.fold` over the same producer (an `uncons` program per element)
114.5. Correctness: a new test in TestGenerate walks a producer with
real interleaved `async` calls, a terminal Async op, a bare produce, a
bare pure, an endless producer under `take`, and a 200k-element
Async-interleaved walk, against the default `unfold` walk as the
oracle, counting that both perform every Async op.

After producer-to-writer-carrier no module walks a G-effectful
producer, so this prices only a consumer of `Source.toProducer` under
G — filed as low priority in the wrap-up and closed here because it
was a thirty-line mirror.

Files: src/main/scala/Generate.scala, src/test/scala/TestGenerate.scala,
compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala (two
rows), src/jmh/history.tsv (two rows).
