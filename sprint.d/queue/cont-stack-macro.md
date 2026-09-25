- [ ] cont-stack-macro — specs/cont-stack.md stage 2, Layer 1 A, after
      cont-stack-switch landed Layers 2 and 3 (2026-09-25): `shift`
      becomes an inline macro that reads the body's tree, and a body
      whose every use of `k` is a tail call `k(v)` with `v` not
      mentioning `k` (plain, `if`/`match` branches) is rewritten to
      `delay(() => Pure(v))` — no new node: the runner's loop already
      walks it with no nested frame, no `Reentry`, no count. Proof: 1M
      such shifts on a 128 KB stack with ZERO switches
      (`StackSwitch.switches`); `Effects.handle`'s `shift(k => k(a.a))`
      (HandlerBenchmark.handleCapture) is the first user. NOT covered:
      state-passing bodies `k => s => k(a)(s2)` (PState) — B's shape
      with a function answer, backlog cont-stack-layer1-b. Beside it:
      shrink the runtime's fast path (`Reentry`, `Gauged`, the `Mapped`
      lambda) once cont-stack-ab has priced it.
