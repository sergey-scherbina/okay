## cont-stack-layer1-b — an answer-using shift body is walked by the runner, its pending parts on a heap stack

specs/cont-stack.md plan stage E (Layer 1 B), first slice: the bodies
the tail-body macro (cont-stack-macro) left to the runtime switch
because something is left to do after each call of `k` — `k(1) +
k(10)`, `a :: k(x)`, `s"${k(a)}"`, a block with `val a = k(1)`, a tail
`if`/`match` calling in its branches.

- `ContMacro`: the selective CPS transform (Rompf, Maier & Odersky,
  ICFP 2009). Each `k(e)` becomes `Cont.Body.Call(k, e, s => rest)`;
  every k-free part evaluated ahead
  of a call is bound to a val first (A-normal form), so what ran before
  a call still runs before it. Followed: a block's statements and
  result, tail `if`/`match`, an application's function part and
  arguments in order, ascriptions, inlined expansions; a by-name
  argument is left alone. Anything else — `k` as a value (`xs.map(k)`),
  a conditional not in tail position, `try`, a loop, a lambda — stays
  the opaque leaf, as before.
- `Cont`: `Body` (`Done`/`Call`), `Cps` (an anonymous subclass per
  shift, one allocation), `cps`. `step` carries an explicit `Pending`
  stack and the body being walked as parameters: a `Call` continues
  the program in-loop through the `Reentry`'s fields (no `enter`, no
  frame, the room unchanged), and every exit that returned an answer
  feeds the part on top. A nested runner (`Reentry.enter`, an opaque
  body's own call) starts with nothing pending and returns as before.
  A `Cps` leaf is never absorbed by `bind`/`mapped` — absorbed, it
  would be applied, the direct road again.
- REFUTED ON THE NUMBER, and taken out before landing: the function
  answer — PState's `s => k(s)(s2)` as a `Fun` walked in-loop, its
  application an `Ap` node — was built first and made 1M PState
  operations run on a 128 KB stack with no switch, and read **89 vs
  32 µs on statePara, 2.8x**, three alternating rounds (history.d).
  Ten allocations an operation against the direct road's three, on a
  program Layer 3's exact room already runs switch-free; the count
  road's one switch per ~870 levels costs ~4 µs, nothing beside it.
  A function answer stays direct; the JS angle (no switch there) is
  noted in cont-stack-layer1-c.
- TestContMacro +7, red first: 1M `k(x + 1) + 1` on a 128 KB stack
  overflowed (StackOverflowError) and now answers with ZERO switches;
  multi-shot shapes, evaluation order, an exception after a call, six
  opaque shapes and the function answer still correct.
  TestDocExamplesContStack re-pinned to docs/cont-stack.md's new
  examples (1M answer-using on 128 KB; 20 000 opaque `map(k)` on the
  switch). Core 548 green.
- Measured (MIN of alternating rounds, `jmh-lane.sh`, JDK 26, ref
  master 8411cd62f; history.d `cont-stack-layer1-b`): the plain shape,
  a NEW lane `HandlerBenchmark.contAnswer` (1000 levels of
  `k(x + 1) + 1`), reads **25.4 vs 20.5 µs, 1.24x** walked against
  direct, three rounds within ±0.35 — at **0.81x the bytes** (198 vs
  246 KB/op): the walked road allocates 48 B a level less, so the time
  is dispatch and loop shape, not objects. statePara 1.01 and fib100
  1.00 (two rounds each): nothing else moved. This is the price the
  plan named — robustness, not speed: such a program never touches the
  stack, on a 128 KB thread, on Scala.js where there is no switch to
  fall back on, at 1.24x on a JVM that would have run it direct and
  switched for free. Profiling the 24% is cont-stack-layer1-c's first
  item; a JS-only expansion is the shape if the JVM price is refused.
- Inventory: six compile-time walks in ContMacro, bounded by the same
  tree `rewrite` walks (specs/stack-safety-okay.tsv). Off the sprint;
  the rest of Layer 1 B filed as backlog cont-stack-layer1-c (non-tail
  conditionals, known higher-order functions, visible user functions,
  `direct`, `try`/loops).
