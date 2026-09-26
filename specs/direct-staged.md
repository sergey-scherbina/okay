# direct-staged — a `direct` block with the handler known at the call site

## Overview

Road 2 of specs/continuations-roadmap.md, with its number. A `direct`
block over a program row lowers to `Free` binds and is run by handlers
afterwards; every operation is dispatched at run time by `split`, and
the tree is re-materialised by every `k(x)`. staged-block-lanes
(2026-09-22) measured the alternative on the shape a block inside a
loop actually is — a STATIC run of operations, then one recursive
bind: with the handler's arm chosen per operation at compile time the
same 1 000 operations run in 7.5 µs / 82 968 B against the fused
`Free` fixture's 11.7 / 123 712 (1.55x) and the shipping runners'
15.0 / 154 448 (1.99x). The lane beside it fixed the design: the same
block with the handler passed as a VALUE is 0.89x — static binds buy
nothing, the whole win is the arm being selected by the compiler.

This lane found the mechanism and measured it before designing around
it (`FusionBenchmark.blockFuncInlineMatchR`):

| form | µs | B/op | vs tree |
|---|---|---|---|
| the tree, fused fixture (`blockFreeR`) | 11.7 | 123 712 | 1.00 |
| inline interpreter applied per op: `split` + `match` on a fresh `State.Get()` (`blockFuncInlineHR`) | 11.5–12.3 | 95.7–97.8 K | **1.0x** — C2 removes the operation's allocation and NOTHING else |
| `inline match` on the operation TERM, arm chosen by the compiler (`blockFuncInlineMatchR`) | **8.15** | **84 568** | **1.45x**, within 8% of the ceiling |
| the ceiling, each op written as its shift (`blockFuncStagedR`) | 7.5 | 82 968 | 1.55x |

So: not an inline handler (the test survives), not a typeclass keyed by
the op's type (a lambda call survives; unmeasured, unneeded) — an
`inline match` whose scrutinee is the operation as written in the
block. The compiler reduces it when, and only when, the scrutinee is a
constructor application it can see: `stage(State.Get())` reduces,
`([X] => e => stage(e))(State.Get())` does not ("cannot reduce inline
match with scrutinee: e" — the polymorphic lambda is not beta-reduced
first). That decides where the macro must apply the stage: on the
operation term itself, at the mark.

## Interface

```scala
// core, Staged.scala
object Handled:
  /** a program over Row at answer type R, as a function of its
   * continuation — Func, with the row and the answer in the type so
   * the direct macro can read them off the block's F */
  opaque type Handled[Row[+_], R, A] = (A => R) => R
  given [Row[+_], R]: Monad[Handled[Row, R, *]]   // closure composition
  def run[Row[+_], R, A](p: Handled[Row, R, A])(k: A => R): R
  inline def shift[Row[+_], R, A](inline f: (A => R) => R): Handled[Row, R, A]

/** a row's staged interpreter: ONE object per (row, answer layout),
 * whose `stage` is an INLINE MATCH over the row's constructors. The
 * trait carries the types; the object carries the method — it is
 * called on the object's own type, so it inlines, and the match on
 * the operation term reduces at compile time. */
trait Stager[Row[+_], R]
// a conforming object:
//   object SW extends Stager[State % Int + Writer % String, Answer]:
//     inline def stage[X](inline op: (State % Int + Writer % String)[X]): Handled[Row, Answer, X] =
//       inline op match
//         case State.Get()   => Handled.shift(k => st => k(st._1)(st))
//         case State.Set(s2) => Handled.shift(k => st => k(s2)((s2, st._2)))
//         case Writer.Say(v) => Handled.shift(k => st => k(())((st._1, st._2 :+ v)))

// okay-direct
object Direct:
  /** the block over Handled[Row, R, *]: every marked OPERATION of the
   * row, and every marked leaf program `Free.Inject(op)` (which is
   * what `State.get[S]`, `Writer.tell(w)`, `Reader.ask[R]` inline to),
   * is emitted as `st.stage(op)` — the arm chosen at compile time.
   * `st` is an inline parameter so that its precise type (the
   * object) is what `stage` resolves on. */
  inline def staged[Row[+_], R](inline st: Stager[Row, R])[A](inline block: DirectCtx[Handled[Row, R, *]] ?=> A): Handled[Row, R, A]
```

`Stager.stateWriter[S, W]` ships in core as the canonical object — the
layout `((S, Vector[W]), A)` of `Fused.stateWriter`, so the fixture's
laws apply to it unchanged.

## Behavior

- [x] A `Direct.staged(sw) { … }` block over `State % Int + Writer %
      String`, spelled with the row's own combinators (`State.get[Int]
      .!?`, `State.set[Int](s + x).!?`, `Writer.tell(s"…").!?`) and
      with a raw operation (`(State.Get(): State[Int, Int]).!?`),
      answers what the SAME block text lowered to a Free program and
      run by `State.run(Writer.run(_))` answers — state, log and
      answer — on 300 generated data sets (`TestStaged`; the block's
      SHAPE is static by construction, the data is generated).
- [x] `if` with marks in both branches and a `for x <- xs do` loop
      over marks work in a staged block as in a Free block: the same
      macro, only the lift differs (the agreement law above runs both).
- [x] (v1, superseded the same day by v2 below) A marked program of
      the row that is NOT a leaf (`State.modify(f).!?`) was a compile
      error naming the fix; v2 walks it.
- [x] A marked value of a FOREIGN monad is refused with the same
      "neither … nor" error a Free block gives.
- [x] MEASURED (`okay-direct` `StagedBenchmark`, two rounds × two
      forks, minima): the `direct`-emitted block `stagedDirect` is
      7.50 µs / 85 368 B against the hand-written `stagedHand` 7.69 /
      84 568 — parity within 1%, +800 B per run (the two hoisted
      vals) — and **2.24x** over `freeDirectNested`, the same block
      text as a Free `direct` block run by the shipping runners
      (16.8 µs / 164 928 B).
- [x] The stack limit is STATED and TESTED: a loop of 2 500
      iterations × 4 operations passes on the default stack
      (`TestStaged`), and docs/direct-style.md says what to do past
      it (a Free block under `Cont`).
- [x] Every existing `direct` suite is unchanged — 326 green — and a
      Free block emits byte for byte what it emitted before, except
      one thing it gains: `val _ = m.!?` binds straight into the rest
      (−38 KB on the benchmark's Free block; see Results).

## v2 — compound programs (direct-staged-v2, 2026-09-22)

v1 staged a mark only when the marked term was one operation, and
refused `State.modify(f)`. After inlining, a compound program IS a
tree the macro can read: `Free.Inject(op)`, `Free.Return(a)`,
`Free.Bind(m, x => body)` with the continuation a lambda literal — the
combinators are `inline def`s, `Free.flatMap` is `Bind(this, f)`,
`map` is `flatMap(a => Pure(f(a)))`, and Row's `.at`/`.plus` are
casts. `DirectRow.stageProgram` walks that tree into the binds a block
of marks would emit, so nothing runs inside a staged block that the
compiler did not see.

- [x] Under a mark in a staged block, `State.modify(f)`, a hand-written
      `get.flatMap(s => set(…))`, a for-comprehension over the row
      (`.at[Row]` on each generator, a `_ <-` among them) and
      `State.get.map(f)` agree with the same text as a Free block on
      300 generated data sets — state, log, answer (`TestStaged`).
- [x] What the walker cannot read is refused naming the shape: a
      program built at run time — a def call, a program held in a
      val, `Free.delay`, a continuation that is a value.
- [x] The leaf road is unchanged: `StagedBenchmark.stagedDirect` at
      85 368 B/op, byte for byte (rows `ds2-*`). The first cut of the
      proxy pass also substituted a single-use `val s$proxy = i + 2`
      into `Set(s$proxy)`; that made the inline match's scrutinee an
      expression the inliner bound to a val, and the operation was
      allocated at run time — 86 968 B, +16 per block. The floor lane
      caught it; only pure right-hand sides are substituted now.

What the walk had to learn, in order: the inliner's proxies are
substituted in one pass first (`proxyFree`) — a pure right-hand side
(a lambda literal, a literal, a name, a program node, an operation)
always, any other only when used once and not under a lambda — so
that `Inject(a$proxy)` and `Bind(Free_this, f$proxy)` read as the
constructors they name; a lambda's `Block(DefDef, Closure)` is left
whole by `unwrap`; a continuation's body is walked BEFORE its bind is
emitted, against a fresh name for the parameter (a failed walk inside
the bind's quote would be a cast exception, not a refusal); and `_ <-
m` in a for-comprehension lands as `() match { case () => rest }`, a
match with one irrefutable case, which is its right-hand side.

## Out of scope

- Any other row than the ones a `Stager` object is written for. The
  layout of the accumulator is the row's business and a type-level
  product over an arbitrary union is a macro of its own; v1 ships
  the mechanism and ONE canonical object, and a user's row is a
  five-line object of the same shape.
- The runtime fallback for compound marked programs (v2).
- Stack safety for staged blocks. `Func` is the fast carrier, `Cont`
  the safe one; that is the library's existing rule and this spec
  does not bend it.
- Auto-coloring (marks omitted) in staged blocks — untried; the mark
  road is the road.

## Design

The whole change is WHERE the operation goes when the macro binds a
mark. For a Free block it is `Free.Inject[Row, X](op)` (DirectRow
.injectTerm). For a staged block it is `st.stage[X](op)`, built by
reflection (`Select` on the stage term's own type, so the object's
inline member is what resolves), and the inliner runs on the macro's
output, so the `inline match` in `stage` sees `State.Get()` and
reduces. A marked LEAF PROGRAM is the same case one layer down: after
inlining, `State.get[Int]` is `Free.Inject.apply[Row, Int](Get())`
under `Inlined`/`Typed` wrappers, so the macro strips the wrappers,
recognises `Free.Inject.apply`, and stages its argument. Everything
else the compiler does — ANF hoisting, if/match, loops, the defer
pre-pass, vals — is untouched: the emission still speaks the
monad's `flatMap`/`pure`, now `Handled`'s.

`rowOf` gains its second case: `F[Unit]` is `Handled[Row, R, Unit]`
(opaque, so it does not dealias to a function type) → `Row`.

## Decisions

- **Explicit stage object, not a given** — chosen because the inline
  member must be resolved on the object's own type: a `given Stager[Row,
  R] = SW` widens to the trait, where `stage` is not a member (an
  abstract inline member is not allowed), and an `inline given` is not
  unwrapped by `Expr.summon`. Passing the object as an `inline`
  parameter keeps its singleton type in the macro. Rejected: `given`
  resolution with a singleton-typed given (a convention the user would
  have to know).
- **`inline match` on the term, not a per-constructor typeclass** —
  chosen because it is the form measured at 1.45x; a typeclass keyed
  by the op's type would put a lambda call where the arm is, and it is
  unmeasured. Rejected until a lane says otherwise.
- **v1 refuses compound programs** — chosen so that a staged block's
  cost is what the mark says: every mark is one arm, no interpreter
  runs inside. The refusal names the program.
- **Stack: `Func`'s contract, stated** — chosen over any trampolining
  of the answer type, which would put a thunk per operation back and
  is the tree by another name.

## Results

**2026-09-22, 5a3af433** (history rows `ds-*`):

| lane (okay-direct `StagedBenchmark`, 1 000 ops, minima) | µs | B/op |
|---|---|---|
| `freeDirectNested` — the same text as a Free `direct` block, shipping runners | 16.8 | 164 928 |
| `stagedDirect` — `Direct.staged(sw) { … }` | **7.50** | **85 368** |
| `stagedHand` — `sw.stage(op)` at every operation, by hand | 7.69 | 84 568 |

Parity to within 1% and 800 B; 2.24x over what the user has today.

**What it took, in the order it was found — four gaps between "the
mechanism works" and the number, each one a macro-output shape:**

1. *The proxy.* `effect(Get())` inlines with a proxy val for its
   argument — `val a$proxy = Get(); Inject(a$proxy)` — and an inline
   match cannot reduce on a name. The op is taken from the marked
   term BEFORE `compile` flattens its `Inlined` wrappers into block
   statements, and the proxy's right-hand side is put back where the
   op stands (`DirectRow.injectedOp`; the name arrives as
   `Inlined(None, Nil, Ident)`, stripped first).
2. *`val _ = m.!?`.* It desugars to `m.!? match { case _ => () }`, and
   the general road bound the mark, matched into a `pure(())`, and
   bound THAT: five objects per statement. Now a discard binds
   straight into the rest — for every block: the Free lane went
   203 328 → 164 928 B, the staged 176 568 → 152 568.
3. *The bind itself.* 152 568 against 84 568 with an identical tree
   before inlining: the hand program's `val M = summon[…]` has the
   GIVEN'S CLASS type (`summon` answers `x.type`), so `M.flatMap` is
   the `override inline` member and reduces; the macro hoisted
   `mm$direct: Monad[F]`, the trait, a virtual call with its closures.
   For a staged block the val keeps the precise type and the calls
   are built by `Select` on it; the lambdas stay quoted (a reflected
   `Lambda` left LambdaLift a `$anonfun` it could not own).
4. *Not for every carrier.* The precise type tried on all blocks broke
   two: `ctxMonad[E]`'s declared result `E ?=> A` is a type the typer
   auto-applies (Erasure "bad adapt", TestDirectTryCtx), and a Free
   block that REBUILDS a lambda (`programLambda`, a nested block under
   `Delim.shift`, TestBookInTheSystem) stranded the inlined binds'
   proxies. So every non-staged block keeps its road; the Free gain
   is filed (`direct-inline-bind-free`) with those two as its laws.

The decisive lanes before the design (core `FusionBenchmark`): the
inline interpreter applied per operation (`split` + `match` on a fresh
op) is **1.0x** — C2 removes the allocation and nothing else; the
`inline match` on the operation term is 8.15 µs / 84 568 B, 1.45x —
and does not reduce through a polymorphic lambda (`[X] => e =>
stage(e)`), which is why the macro applies the stage to the term.

**2026-09-26, handlers-vs-plain-loop** (history.d
`handlers-vs-plain-loop`): the baseline this table lacked — the same
1 000 operations as a plain `while` loop with a `var` state. With the
same persistent `Vector` log the staged answer threads it reads 2.79
µs / 34 136 B, so `stagedDirect` (7.79 / 85 368) is **2.79x** the loop
and the Free block (14.2 / 164 928) **5.10x**; with a mutable
`ArrayBuffer` log, 0.706 µs / 4 192 B, the factors are 11x and 20x.
The staged road is the closest a library gets to a compiled handler,
and it is not a plain loop: what is left is the continuation closure
per operation. docs/benchmarks.md §2c.
