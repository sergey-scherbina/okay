## direct-staged-v2 - compound programs under a mark are staged, not refused

`Direct.staged` v1 staged a mark only when the marked term was one
operation and refused `State.modify(f)`. After inlining, a compound
program is a tree the macro can read — `Free.Inject(op)`,
`Free.Pure(a)`, `Free.Bind(m, x => body)` with the continuation a
lambda literal; the combinators are `inline def`s, `Free.flatMap` is
`Bind(this, f)`, `map` is `flatMap(a => Pure(f(a)))`, Row's
`.at`/`.plus` are casts. `DirectRow.stageProgram` walks that tree into
the binds a block of marks would emit: `State.modify(f)`, a hand-written
`get.flatMap(s => set(…))`, a for-comprehension over the row with a
`_ <-` among its generators, and `State.get.map(f)` all agree with the
same text as a Free block on 300 generated data sets (`TestStaged`).
A program the walker cannot read — a def call, a val, `Free.delay`, a
continuation that is a value — is refused naming the shape.

Four things the walk had to learn, each found by a failing shape:
the inliner's proxies are substituted in one pass first (a pure
right-hand side always, another only when used once and not under a
lambda); a lambda's `Block(DefDef, Closure)` is left whole; a
continuation's body is walked before its bind is emitted, against a
fresh name (a failed walk inside the quote was a cast exception, not a
refusal); `_ <- m` lands as `() match { case () => rest }`.
`bind`/`pureF` are declared on `DirectPhase` so the row phase can emit
without extending emission. The leaf road is byte-identical.
