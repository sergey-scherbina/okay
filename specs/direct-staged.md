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
object Staged:
  /** a program over Row at answer type R, as a function of its
   * continuation — Func, with the row and the answer in the type so
   * the direct macro can read them off the block's F */
  opaque type Staged[Row[+_], R, A] = (A => R) => R
  given [Row[+_], R]: Monad[Staged[Row, R, *]]   // closure composition
  def run[Row[+_], R, A](p: Staged[Row, R, A])(k: A => R): R
  inline def shift[Row[+_], R, A](inline f: (A => R) => R): Staged[Row, R, A]

/** a row's staged interpreter: ONE object per (row, answer layout),
 * whose `stage` is an INLINE MATCH over the row's constructors. The
 * trait carries the types; the object carries the method — it is
 * called on the object's own type, so it inlines, and the match on
 * the operation term reduces at compile time. */
trait Stage[Row[+_], R]
// a conforming object:
//   object SW extends Stage[State % Int + Writer % String, Answer]:
//     inline def stage[X](inline op: (State % Int + Writer % String)[X]): Staged[Row, Answer, X] =
//       inline op match
//         case State.Get()   => Staged.shift(k => st => k(st._1)(st))
//         case State.Set(s2) => Staged.shift(k => st => k(s2)((s2, st._2)))
//         case Writer.Say(v) => Staged.shift(k => st => k(())((st._1, st._2 :+ v)))

// okay-direct
object Direct:
  /** the block over Staged[Row, R, *]: every marked OPERATION of the
   * row, and every marked leaf program `Free.Inject(op)` (which is
   * what `State.get[S]`, `Writer.tell(w)`, `Reader.ask[R]` inline to),
   * is emitted as `st.stage(op)` — the arm chosen at compile time.
   * `st` is an inline parameter so that its precise type (the
   * object) is what `stage` resolves on. */
  inline def staged[Row[+_], R](inline st: Stage[Row, R])[A](inline block: DirectCtx[Staged[Row, R, *]] ?=> A): Staged[Row, R, A]
```

`Stage.stateWriter[S, W]` ships in core as the canonical object — the
layout `((S, Vector[W]), A)` of `Fused.stateWriter`, so the fixture's
laws apply to it unchanged.

## Behavior

- [ ] A `Direct.staged(SW) { … }` block over `State % Int + Writer %
      String`, spelled with the row's own combinators (`State.get[Int]
      .!?`, `State.set(i).!?`, `Writer.tell("w").!?`) and with raw
      operations (`State.Get().!?`), answers what `Fused.stateWriter`
      answers on the same program — value, state and log — on a
      scalacheck-generated interleaving (≥ 300 cases).
- [ ] `if`/`match` with marks, marks in subexpressions (hoisted in
      order), and a `for x <- xs do` loop over marks work in a staged
      block exactly as in a Free block: the emission is the same
      macro, only the lift differs.
- [ ] A marked program of the row that is NOT a leaf (`State.modify(f)
      .!?`, a `def` returning `A ! Row`) is a compile error naming
      the program and the fix (perform its operations in the block).
      v1 refuses; the runtime fallback (interpret the program through
      the same stage) is v2 and is not built until asked for.
- [ ] A marked value of a FOREIGN monad, or an operation outside the
      row, is refused as in a Free block (the same error).
- [ ] MEASURED: the `direct`-emitted block on the benchmark's 1 000-op
      shape (`FusionBenchmark.directStagedR`) matches the hand-written
      `blockFuncInlineMatchR` — 84 568 B/op to the byte, µs within
      5% — and stays ≥ 1.4x over `blockFreeR`.
- [ ] The stack limit is STATED and TESTED: a staged block is `Func`,
      not stack-safe on a left-nested chain; a loop of 10 000
      operations passes on the default stack, and the docs say what
      to do past it (run under `Cont`, i.e. a Free block).
- [ ] Every existing `direct` suite is unchanged: the seam adds a
      second case to `rowOf` and a second lift; a Free block takes
      the same path it took.

## Out of scope

- Any other row than the ones a `Stage` object is written for. The
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
monad's `flatMap`/`pure`, now `Staged`'s.

`rowOf` gains its second case: `F[Unit]` is `Staged[Row, R, Unit]`
(opaque, so it does not dealias to a function type) → `Row`.

## Decisions

- **Explicit stage object, not a given** — chosen because the inline
  member must be resolved on the object's own type: a `given Stage[Row,
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

(after the lane)
