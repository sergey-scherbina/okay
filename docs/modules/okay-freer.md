# okay-freer

THE CLASSIC: the effect library over the freer tree as it was in the core, now a module of its own, package
`okay.freer`, ABOVE the core — one `Effects` instance of the core's interface, chosen at compile time by the
given in scope, knowing nothing of the machine (okay-cont) but through that interface. It depends on the core
and on nothing else; it runs on the JVM, Scala.js and Scala Native (specs/freer-min.md, stage 45;
specs/modules-infra.md).

| | |
|---|---|
| `A ! F`, `pure`, `effect`, `op.perform` | a program of the tree: `Free[F, A]`, the effect tree at the unary signature over `Freer[G, S, R, A]`, the indexed tree (Kiselyov–Ishii, with Atkey's indexes) |
| `Classic[M]` | the classic as a typeclass, over its union rows: level 0 — `pure`, `perform`, `flatMap`, `foldCont` into the carrier, `handle` by a clause — and level 1 — `shift`, `shift0`, `reset`, `handle(m, h)`, `run`, `foldMap` — for a program written once over any encoding of the tree; `Free` and `Eager` are its instances |
| `Rowed[R, A]`, `classic` | the tree under the core's facade: its instance of `Effects[M]`, the interface over nominal rows — operations tagged with their path, the machine's handlers run on the machine and reified back into the tree; `import okay.freer.tree.*` gives it the facade's words (`A ! R`, `effect`, `handle`, `value`) |
| `Classic`, `!` for short | the classic as a toolkit: `!.run`, `!.relay`, `!.translate`, `!.interpret`, `!.foldM`, `!.loop`, and the tree's constructors |
| `given_Classic_Free`, `FreeEffects`, `cps` | the tree's `Classic` instance with the machine as its carrier; `import okay.freer.cps.{given_Classic_Free, *}` chooses the CPS `Cont` as it was |
| `Handler`, `Answers`-handlers, `Row`, `Member`, `Distinct`, `Row.union`, `Row.flat` | handlers as values, rows as unions, their evidence; `split` and `TypeableK[Pure]`, the class test a union handler dispatches by |
| `State`, `Reader`, `Writer`, `Throws`, `Choose`, `Shift`, `Resource`, `Once`, `Supply`, `Random`, `Clock`, … | the effects and their handlers |
| `Cont[A, S, R]`, `Delimited`, `StackRoom`, `StackPool` | the CPS tree at the shift signature, and the classic's machine of delimited continuations (the JDK 22 StackRoom variant is in this module's multi-release jar) |
| `Stream`, `Gen`, `Producer`, `Aggregator`, `Fold`, `Chunk` | the streams and folds written over the tree |
| `DirectCtx`, `Diagonal`, the macros | what `okay-direct`'s `direct` block and the handler macros need, under `okay.freer.macros` |

What stays in the core (`okay`): `Effects` over rows and the machine's instance of it, the program `A ! R`
(the machine's `Free`) with `pure`, `effect`, `perform`, `handle`, `value`, the rows at its door, `Control`,
`Answers`, `TypeableK`, `Effect`, the type classes (`Monad`, `TailRecM`, …) — the interface and the machine,
with no classic in it. A file takes the core's names BY NAME (`import okay.{Answers, Effect, Monad}`) beside
`import okay.freer.*`: the two have `!`, `+`, `%`, `Pure`, `pure`, `effect`, `perform` each, the machine's and
the classic's, and a wildcard would make them tie — or, in a file of package `okay.freer`, win.

## Using it

```scala
import okay.freer.*
import okay.freer.given

val p: Int ! State % Int = State.get[Int].map(_ + 1)
val j = p.handle(State(41)).run   // (41, 42)
```

Code written over any encoding of the tree takes `Classic[M]`:

```scala
def program[M[_[+_], _]](using E: Classic[M]): M[State % Int, Int] =
  E.reset[Int, State % Int](
    E.shift[Int, Int, State % Int](k => k(1).flatMap(a => k(10).map(b => a + b))).flatMap(x =>
      E.perform[Shift % Int + State % Int, Int](State.Get[Int, Int]()).map(s => x * 2 + s)))

val inFree = summon[Classic[Free]].run(summon[Classic[Free]].handle(program[Free], State(5)))      // (5, 32)
val inEager = summon[Classic[Eager]].run(summon[Classic[Eager]].handle(program[Eager], State(5)))  // (5, 32)
```

A macro or a tool that names the tree by its path names it at its home: `okay.freer.Freer`, `okay.freer.Free`,
`okay.freer.Diagonal`, as `okay-direct`'s do.

## Two wildcards

A file in package `okay` used to see the classic's names as package members and any `import x.*` outranked
them. Now the classic is a wildcard import too, and two wildcards tie — the places that met it, and how they
read now: `!.*` no longer exports `Free.pure` (the top-level `pure` is the one); the tree's `foldMap` lives in
`Freer`'s companion, so `Optic`'s is not shadowed; a direct-style test names `Direct`'s `shift` and `reset`
(a named import outranks a wildcard); a `Chunks` test hides the program's `toLazyList`; a file in package
`okay` that means `okay.macros` writes it in full, since `okay.freer.*` brings `macros` too.
