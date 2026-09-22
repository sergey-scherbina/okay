- [ ] typeablek-instanceof — a derived signature's test is
      `Effect.ByClass(cls).test(x) = cls.isInstance(x)`: a
      `Class.isInstance` on a class loaded from a FIELD, where the
      hand-written match the JIT sees compiles to a constant-class
      `instanceof`. Measured as the residual of handler-fusion-flat
      (FlatDispatchBenchmark, position 4, minima): the macro's chain of
      `t.test(a)` 100.2 µs against the same chain written as
      `case e: E1[A]` 94.9 — 5.6% on a lane that is nothing but
      dispatch. That test is under EVERY `split` in the library
      (State.handle, Writer.foldWith, relay, Effects.handle, union,
      flat), so the prize on a real runner is that 5.6% times the
      share of a pass the test is — small, uniform, and worth one
      lane. SHAPE: `Effect.derivedImpl` emits, per `derives Effect`
      site, a class whose `test` is `x.isInstanceOf[F[?]]` on the
      signature's erasure — one class per signature, not per use
      (the reason `Effect.of` is not inline still holds). `ByClass`
      stays for `typeableK(cls)` where the class is a run-time value.
      NUMBER: `fusedSWr` (the floor, 122 640 B/op — must not move in
      bytes) and `nestedSWr`, `relayForward`, `inline4` before/after,
      alternating rounds; under 2% on the runners is a refutation
      worth recording, not a win.
