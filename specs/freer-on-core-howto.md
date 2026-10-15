# Moving a module onto the kernel's words: how

Companion of [freer-on-core.md](freer-on-core.md) (the why and the mapping) and
[freer-on-core-order.md](freer-on-core-order.md) (the order). Converted examples to copy from: okay-std (main),
okay-async (`Async.scala`: a drive over the machine's head), okay-platform.

## The shortest road: change the imports

The root keeps okay-freer's names as wrappers over its own words (`src/main/scala/Compat.scala`): `!.run`,
`!.tailcall`, `!.widen`, `!.loop`, `!.translate`, `!.interpret`, `Row.plus`/`at`/`up`/`union`,
`Free.pure`/`delay`/`loop`, `op.perform`, and `Distinct` always given. So first change `okay.freer.X` to `okay.X`
in the imports (`import okay.freer.*` and `.given` to `import okay.*`) and compile; most of a module compiles
then. What is left is what the wrappers cannot keep:

- `pure[F, A]` (effect first) is `pure[A, F]` on the root.
- `effect[Row, X](op)` with a whole row named: `effect(op)` (it widens where needed).
- A runner's signature over a rest inferred by the union (`A ! E + F => B ! F`) takes `Split` (below).
- Code that walks the tree (below).
- A program's `Monad` (generic `traverse`/`sequence`) by `import okay.Programs.given` — never into a scope where a
  program's own `flatMap` is used: its one-row `flatMap` takes the bind from `Run.flatMap`.

## The words

- Imports: `import okay.freer.*`, `import okay.freer.given`, `okay.freer.Row.{plus, at, up}`, `okay.freer.{...}`
  go. In `package okay` the root's words are there already; elsewhere `import okay.*` or the names
  (`import okay.{!, +, %, pure, effect, defer, Handler}`).
- `A ! F + G`, `F % S`: the same syntax. An effect's kind is `F[_]`, not `F[+_]`.
- `pure(a)`, `effect(op)`, `defer(p)` (was `Free.delay`/`suspend`/`tailcall`), `p.flatMap`, `p.map`, `p >> q`.
- A program needing fewer effects fits where more are expected by itself (the `widen` conversion); to say it,
  `p.at[Es2]`. `.plus[F]`, `Row.at`, `.up`, `.widen` go.
- `p.run` on `A ! Pure`. `p.runWith(using answers)` as before: each effect answered in place, a sum's `Answers`
  assembled from its parts (`Answers.sum`).
- A RUNNER takes its effect off wherever it is:
  `def run[A, F[+_]](p: A ! E + F): B ! F` becomes
  `def run[A, Es[_]](p: A ! Es)(using s: okay.core.Split[Es, E]): B ! s.Rest = p.handle(h)`.
  `A ! E` alone (a runner of the last effect): the same signature, `s.Rest` is `Pure`.
- A HANDLER is a value polymorphic in the body's answer: `Handler[E, O]`, `Handler.answering`, `.state`, `.deep`,
  `.aborting`; or a kernel form written out (`okay.one.handler.{Answering, Stateful, Deep, Aborting,
  Translating}`) as in okay-std's `Reader.apply`.
- Interpreting one effect into another: `p.translate[E, F](f)` (`A ! F + rest`), `p.translateInto[E, F](f)` when
  `F` is among the rest already (one place, not two). `!.translate`, `!.interpret` go.
- Intercepting (a handler of `E` for `p` whose own operations go to the `E` outside): `p.intercept(h)`.
- Evidence that goes: `TypeableK`, `Distinct`, `Handler.Nothing`, `A <:< Any`, `Row.In` (the kernel finds a path,
  no class test).
- `Failing` goes: on the kernel a failed operation is raised where it was (`k.failed`), inside the guards.
- `Async`: `Async.run(p)` is a runner (Split); `CancelScope.enter`/`exit` have no type parameter; a drive's
  rest is `Async.Rest[A]`.

## Walks over the tree: `okay.Walk`, one way for all

Code that looked at the tree (`p.resume match { case Return(a) …; case Bind(Inject(e), k) … }`, `HandleFrames`,
`Classic.handle`, `next`, `peek`) walks by the root's `okay.Walk` (src/main/scala/Walk.scala) — okay-freer's
`resume` on the kernel, the one walking machinery of the platform. Do not write a stepper of your own.

- `Walk.view(p)` — `p` needing `E` alone: `Walk.Done(a)`, or `o: Walk.Op[E, x, A, E]` with `o.op`,
  `o.resume(x)` (the rest, a program again) and `o.fail(e)` (the rest given a failure raised where the operation
  was). A driver loops `Walk.view(o.resume(answer))`; walked again at the same place the rest goes on from where it
  was, nothing re-fitted, one object a step.
- `Walk.next[E, A, Es](p)` — `E` among other effects: `Step[E, A, Es] ! s.Rest`, the others' operations performed
  where the walk is (okay-freer's `relay` forwarding).
- The rest is a value: resumed any number of times, from any thread; run at another place its operations are
  re-placed by reading the two places' paths.

Before walking at all, ask whether a kernel handler form does it — most walks were interpreters: a per-operation
answer is `Answering`, a state threaded is `Stateful`, a clause with the rest is `Deep`, an operation to another
effect is `translate`. Those are faster than any walk: answered in place, nothing captured.

## Rules

- No unbounded recursion (AGENTS.md); no cast without necessity, one isolated with a comment where it must be.
- No warnings in a file you touched (unused imports, unused pattern variables: `?`).
- Behaviour that changes: write it in the report, and in freer-on-core.md's "Known changes" when it is real.
- A test of okay-freer's own machinery (the tree's shape, `resume`, `HandleFrames`, `Distinct`, `TypeableK`, union
  routing) has no counterpart: it is removed from the branch (it stays at the tag `classic-freer`); a test of the
  module's API is converted and kept green.
- Never edit okay-freer, okay-core (the submodule), okay-std's main, or the root (`src/`) without the lead's word:
  ask for a word that is missing.
- Build: `sbt --client "<project>/compile"`, then `Test/compile`, then `testOnly` of the module's suites; JS and
  Native for a cross module. The sbt server is shared: never `clean`, `reload` or `shutdown`, no JMH.
