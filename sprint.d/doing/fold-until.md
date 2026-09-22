- [~] fold-until — a fold that can STOP. `Fold[A, S]` is init + add
      and walks to the end by construction; its own `exists` says so
      ("a Fold has no way to stop, which is what `Chunks.exists` is
      for") and `Chunks.exists` does not exist — the comment cites a
      phantom (grep `def exists` in okay-stream: nothing). The shape
      the operator brought (2026-09-22): a state that decides when the
      iteration ends, `S => Either[S, R]`, i.e. `tailRecM` at Id and a
      catamorphism with a halt. Spec: specs/fold-until.md, four
      stages; THIS lane is stage 1 — `FoldUntil[A, S, R]` in core
      Fold.scala (`step(s, a): Either[S, R]`, `end(s): R`), the
      library instances that were missing (`find`, `headOption`,
      `takeN`, a stopping `exists`/`forall`), and one consumer per
      carrier: `Stream.foldUntil` (any Stream via its iterator),
      `Chunks.foldUntil` (per-chunk `while`, the Either built once
      per chunk boundary at most — never per element), `Writer.
      loopUntil` + `Source.runFoldUntil` (the tail-recursive walk with
      an early `Pure`). Laws: agreement with `foldLeft` + `takeWhile`
      on the pure road; the walk STOPS (a source counting its
      productions is asked for 3 of 1000 and produces 3 + at most
      one chunk); a forwarded Async op before the stop is performed,
      one after it is not. Stages 2–4 (`loop` on `!`, a stopping
      `transduce`, the `foreach`/`Foldable` extension) stay in the
      spec as `- [ ]` with their triggers.
