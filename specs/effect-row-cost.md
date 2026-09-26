# effect-row-cost — what a level of an effect-typed program costs, and the fixes

## Overview

stack-safe-mutual-recursion measured the same 1 000 000-level mutual tail
recursion — count every level, log every 1 000th — with the effects as
interfaces and as a row (compare `MutualRecursionFxBenchmark`, 2026-09-26):

| road | per level |
|---|---|
| okay, effects as interfaces over `!.tailcall` | 4.0 ns, 48 B |
| okay, effects in the type, `State.run(Writer.run(p))` (`okayRow`) | 35 ns, 376 B |
| cats Eval with the same interfaces | 2.5 ns, 40 B |

The road the docs teach costs nine times the road they call a workaround.
This spec takes the gap apart and fixes what it can.

## Results so far — where the bytes go

`ProbeRowCost` (exact bytes per level, `ThreadMXBean`, warm, N = 100 000;
load-independent, so it could be read while the box was busy):

| variant | B/level |
|---|---:|
| the row as written, `State.run(Writer.run(p))` | 384 |
| the same program, `Writer.run(State.handle(p))` | 240 |
| State alone, `modify` (get + set) | 240 |
| State alone, one `set` | 96 |
| State alone, one `get` | 80 |
| `!.tailcall`, no effect | 40 |

Two causes, each read in the code and now measured:

1. **Forwarding: 72 B per forwarded operation, 144 a level.** The inner
   `Writer.run` meets every State operation and forwards it as
   `Inject(e).flatMap(x => _loop(s)(k(x)))` — a new `Inject`, a `Bind`,
   a closure over `s` and `k` — before the outer `State.handle` sees it.
   Handling State first (the swapped order) removes all of it: 384 → 240.
2. **`modify` is two operations**, `get` then `set`
   (State.scala `modify = get.flatMap(s => set(f(s)))`): 240 B against
   96 for one `set` — more than the sum of its parts, because the
   `flatMap` closure over `f` and the boxed new state come on top.

## Results after D1 (ProbeRowCost, exact, the same probe)

| variant | before | after |
|---|---:|---:|
| the row as written, `State.run(Writer.run(p))` | 384 | 168 |
| the same program, `Writer.run(State.handle(p))` | 240 | 96 |
| State alone, `modify` | 240 | 96 |

The row's remaining 72 B over the swapped order is one forward per
level (was two). D2 — reusing the matched `Inject` when forwarding —
would save 16 of those 72 but needs a cast from `Writer % W + F` to
`F` (Free is invariant in its row), and the repo's rule is no cast
without a real necessity: deferred until a measurement shows the 16 B
matter in time.

## Decisions

- **D1. A one-step `Modify` operation in State** (`case Modify(f: S => S)`,
  answering the new state, as `modify` always has). `modify` becomes one
  operation; `State.handle` applies `f` in its loop. Expected: State-only
  `modify` from 240 B to about `set`'s 96 plus the closure, and every
  forwarded counter pays one forward instead of two. Every exhaustive
  match on State's constructors learns the case (the compiler lists
  them: an unmatched constructor is a warning, and warnings are errors
  here). `update`/`swap` stay get + set: they answer from the OLD state,
  which a one-step `S => S` cannot return.
  Alternative considered: a general `Update[B](f: S => (B, S))` as the
  one primitive — rejected for now, it allocates the pair `modify`
  exists to avoid.
- **D2. Forward without re-allocating the operation node.** The loops'
  forwarding arm rebuilds `Inject(e)` although the matched node is that
  very `Inject`; bind it (`case Bind(i @ Inject(e), k)`) and forward
  `Bind(i, …)`. Saves 16 of the 72 B. The closure over `(s, k)` and the
  `Bind` are the residual's continuation and stay.
- **D3. Say which order to handle in.** The frequent effect innermost:
  that is the whole of cause 1 for this shape, and it costs a user
  nothing but knowing. docs (effects guide, State and Writer pages) and
  `Writer.run`'s scaladoc.
- Deferred: a fused State+Writer runner (handler-fusion's pass fusion,
  1.13–1.29x, gated off in 2026-09). Re-measured only if D1–D3 leave a
  large gap.

## Behavior

- [x] `State.modify(f)` is ONE operation: a handler sees `Modify(f)`, and
      `State.handle` answers the new state (TestState pins it: the
      program is a single `Inject(Modify(_))`, red on master first)
- [x] every interpreter of State's operations handles `Modify` (compile
      is the check: seventeen exhaustivity warnings named them —
      State.handle, zoom, Bisim, Lexical x4, the stagers, the test handlers)
- [x] ProbeRowCost: State-only `modify` at most `set` + 32 B a level (96 = set's 96)
- [ ] the forwarding arms reuse the matched `Inject` (ProbeRowCost: the
      row's forwarding cost per operation down by 16 B)
- [x] docs say which handler order to choose, with this measurement (guide, "The order is also a price")
- [x] MutualRecursionFxBenchmark `okayRow` and `okayRowSwapped` re-measured;
      docs/benchmarks.md §2d carries the before and after (33.3 -> 18.2 ms,
      18.1 -> 9.7 ms, min of 3 alternating rounds)
- [x] the handler lanes (docs/benchmarks.md §2 Reader/Writer, §2c) do not
      regress (§2c's freeDirectNested, State get/set + Writer: 14.17 against
      14.22 us, bytes identical; Reader/Writer handlers are untouched)
