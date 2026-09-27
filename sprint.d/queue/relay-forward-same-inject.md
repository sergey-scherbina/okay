- [ ] relay-forward-same-inject — specs/effect-row-cost.md D2, so far
      deferred "until a time measurement shows the 16 B matter": the
      forwarding arms of `relay` (Effects.scala:607), `handle` (:249)
      and `translate` (:566) rebuild `Inject(e)` for an operation
      whose matched node IS that `Inject`. `ReadyMerge` already
      forwards the source's own `Inject(Say)` node (ReadyMerge.scala:
      148-168, merge-cap256-gap) and that was 12-14% of the bytes on
      its lane. Here it is 16 of the 72 B a forwarded operation costs
      per handler level (effect-row-cost, `ProbeRowCost`), i.e. a
      four-effect row where each handler forwards three quarters of
      what it sees. THE CAST it needs is the reason it waited:
      `Free[F + G, X]` to `Free[G, X]` is the union split's excluded
      middle, already claimed once in `split` (Handler.scala:298) —
      make it ONE named door beside `split` (`forwarded`, with the
      same paragraph), not three inline `asInstanceOf`s (operator
      rule: no cast without a real necessity, and when one must go,
      one function with the reason). BAR: `relayForward`,
      `handlePrebuilt`, `okayRow` (the row lane, 18.2 ms) and
      `ProbeRowCost`'s exact count (72 -> 56 B per level); time may
      not move — a bytes-only win is recorded as such and kept only if
      the loops' bytecode stays under `TestInlineBudget` (relay's loop
      is 266 bytes, `resume` 323 of 325: a cast is a `checkcast`, 3
      bytes, but the pattern `i @ Inject(e)` may add a local). If
      `map-flatmap-pair-cost` (in doing) changes the same loops, land
      after it and re-measure on its tree. (2026-09-27, perf-plan)
