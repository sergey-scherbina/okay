- [x] delay-node — LANDED 2026-09-15 (tailcallChain 5.3x, handleCapture
      1.35x, controls identical to the byte; specs/core-cleanup.md
      "delay-node"). The entry as written, for the record: a `Delay(thunk)` case beside `Defer`, so that a
      deferred call with NOTHING to do afterwards does not pay for a
      continuation. Found by the Free/Cont/Effects review (2026-09-15,
      specs/core-cleanup.md Decisions). The mechanism is the one
      `hff-defer-cost` priced: `Defer(t, pure)` resumes to `Bind(t(),
      pure)`, and when `t()` is itself a `Bind` the rotation pushes a
      `.flatMap(pure)` tail down every bind of the deferred subprogram
      — one closure and one `Bind` per bind, then a `Bind(Pure(a), g)`
      chain of the same length at the end. `handle` stopped paying it
      for handlers that ANSWER (handle-forward-fast); it still pays on
      the capturing arm (Throws, Choice), and so do `!.tailcall`,
      `Effects.tailcall`, `Eff.flatMap` (every bind of the Church
      encoding is a `Cont.defer`) and the codecs' trampolines past
      `NativeThreshold` (`Cont.defer(...)(Cont.Pure)`). With `Delay`:
      `case Delay(t) => t().resume`, `case Bind(Delay(t), g) =>
      Bind(t(), g).resume` — no composition, no tail.
      WHY NOT JUST DONE: it is a fifth case in `Free.resume` and
      `Cont.step`, the two loops whose bytecode shape has already cost
      1.44x (shrinking `resume` under FreqInlineSize, 409c06e2) and 10%
      (`relay` four bytes over it) — and the `mapnode` row (2026-08-29)
      is a case added to this interpreter that lost 1.22x on fib100
      "despite strictly less work per element". So: (1) a JMH lane
      that HAS the shape — a `tailcall` chain of depth 10 000 and a
      capturing handler over the 10k-op tree (`hff-defer-cost` read
      210 vs 151 µs for it); (2) `Delay`, with `tailcall`, `handle`'s
      capturing arm, `Eff.flatMap`/`defer` and `Cont.defer`-with-Pure
      on it; (3) fib10/50/100/1000, relayPrebuilt, handlePrebuilt,
      statePara as controls, `-f 3`, three rounds. Lands only if the
      controls hold; a refutation with numbers is a fine outcome.
      Typed route, no cast: a `Defer(t, null)` or an identity-compared
      shared `pure` continuation were considered and refused.
