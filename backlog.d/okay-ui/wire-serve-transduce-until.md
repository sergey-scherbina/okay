- [ ] wire-serve-transduce-until — `Wire.serveClosing` (okay-ui,
      Wire.scala) is a hand-written `loop`/`step` pair over
      `Stage.await`: state `(vocab, s, shown)`, stops on `Msg.Close`
      or a `done` update, tells patches in between — exactly
      `Stage.transduceUntil(z)(step, end)`'s shape (specs/fold-until.md
      stage 3). Found by loop-audit (2026-09-23) counting the `!.loop`
      doors; not rewritten there because it is a `Stage`, not a `!`
      program. Rewrite over `transduceUntil` with the protocol suite
      unchanged; the `step`'s "damage dropped" and "forged dropped"
      arms become `Left(state)`.
