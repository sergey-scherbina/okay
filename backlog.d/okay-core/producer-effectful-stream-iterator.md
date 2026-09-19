- [ ] producer-effectful-stream-iterator — `Stream[[A] =>> A ! Produce +
      G, G]` (src/main/scala/Generate.scala ~273) has no `iterator`
      override, so a G-effectful producer walks through the default
      `Iterator.unfold(s)(uncons(_).runWith)` — an Option + Either +
      Free node per element, the tax writer-stream-specialized-iterator
      removed from `writerStreamIn` (6.29 -> 5.43 us, 32,952 -> 12,688
      B/op on 157 chunks). Low priority: after producer-to-writer-
      carrier nothing in main walks a G-effectful producer; it matters
      only to a consumer of `Source.toProducer` under G. The fix is
      the same ~30-line mirror of `writerStreamIn`'s override, with
      `produced[A](e)` where that one matches `Say`. Measure before
      claiming a number (the memory benchmark-loop-placement).
