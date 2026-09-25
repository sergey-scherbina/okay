## cont-stack-macro — a tail-shaped shift is the value it passes, decided at compile time

specs/cont-stack.md plan stage B (Layer 1 A).

- `shift` is an inline macro (`ContMacro`). A body whose every use of
  `k` is a tail call `k(v)`, `v` free of `k` — through a block's
  result, `if` branches, `match` cases, ascriptions, inlined calls; a
  `throw` in tail position allowed — becomes `Cont.tailShift(() => v)`,
  a `Delay` the runner's loop walks, or `Cont.tailPure(v)` when `v` is a
  literal or a stable name with nothing before it. No leaf, no
  `Reentry`, no nested frame, no room, no switch. Any other body is
  `Cont.shiftLeaf(f)`, the tree `shift` always built.
- `Reader.local`'s clause `shift(k => k(r2))` is the library's user: a
  bare `Return` per `Ask`. (`HandlerBenchmark.handleCapture`, which the
  plan named, uses Free's `!.shift`.)
- `given Control[Cont]`'s `shift` calls `shiftLeaf`: its override keeps
  a retained non-inline body, and the macro expanded there made a
  suspension cycle with the file defining the types it reads — "stale
  symbol Cont$" on every compile, clean included.
- TestContMacro (6, red first: 1M tail shifts on 128 KB overflowed or
  switched); TestCont, TestContStack and TestContStackNative build
  their leaf with `shiftLeaf`, since a tail body no longer exercises
  the leaf or the switch. Gate: affected staged, 5 309 JVM + 2 021
  JS/Native, no warnings.
