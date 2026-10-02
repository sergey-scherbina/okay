# `shift`/`reset` as an effect: level 1 of the API

Operator ask, 2026-10-02. The library has three audiences:

1. **the user** — uses ready effects and continuations, writes no effect
   of their own. Knows `A ! F` (the row `F + G`), `pure`, `perform`,
   `shift`, `reset`, `handle`, and nothing else;
2. **the effect author** — declares a signature, writes its handler;
   sees `Cont` (a handler's clause is a `Cont[X, B ! G, B ! G]`),
   `Delim`'s named prompts, `split`, `relay`;
3. **the library** — `Freer`, the machines, the macros.

Today a level-1 user meets TWO `shift`/`reset` pairs: `okay.shift` /
`okay.reset` (`Cont`: one prompt, answer-type modification, no effects
between the two — `k: A => S` returns a VALUE) and `Delim.shift` /
`Delim.delimited` (`R ! Delim + F`: effects anywhere, `k` returns a
PROGRAM, the answer type from a `Prompted[R]` evidence). This spec is
the probe for ONE pair living in `A ! F`.

## Overview

A continuation is an effect in the row, and `reset` is its handler —
the standard encoding of `shift`/`reset` by a deep handler (Forster,
Kammar, Lindley & Pretnar, "On the expressive power of user-defined
effects", JFP 2019):

```scala
final case class Shift[R, +A](body: (A => R ! ?) => R ! ?)    // the row erased
def shift[R, A, F[+_]](f: (A => R ! F) => R ! F): A ! Shift % R + F
def reset[R, F[+_]](body: R ! Shift % R + F): R ! F
```

- **the row writes `Shift % R`**, as it writes `State % S`: the answer
  type is the effect's parameter.
- **the body's type is `R ! F`, without `Shift % R`.** So the body cannot
  capture to its own `reset` again, and then `shift` and `shift0` cannot
  be told apart. The body runs outside the handler (shift0's placement), and `k`
  is deep, `λx.reset(K[x])` (shift's `k`), which is Danvy–Filinski's
  `shift` for every body that does not shift. One `reset` per capture,
  not one per body, and no quadratic re-walk.
- **two answer types in one row are refused** by `Distinct`, the rule
  every effect already follows (`Shift % Int + Shift % String` tests by
  class). Nested `reset`s whose bodies keep to their own row are fine;
  the innermost `reset` of the class answers, as any handler does.

Level 2 keeps `Cont[A, S ! F, R ! F]` beside it, which is not new:
`Monadic.reflect` (`shift(k => m.flatMap(k))`) lifts an effect into it,
`Monadic.reify` (`c / pure`) is its `reset`. On the diagonal the two
forms are one thing:

```scala
p.cont : Cont[A, R ! F, R ! F]   // from A ! Shift % R + F: reset's fold, not yet run
c.!    : A ! Shift % R + F       // a whole Cont as ONE shift: shift(k => c / k)
```

and off the diagonal (`S ≠ R`) `Cont` is the only home of answer-type
modification.

## Behavior

The probe — `okay-direct` test sources, both implementations behind
one API, one suite over both:

- (a) `Shift % R` as an ordinary effect, `reset` a deep handler over
  `Effects[Free].handle`.
- (b) the same API on `Delim`'s machine: `shift` is `Delim.shift0` to
  one shared prompt (the innermost installed one answers, as (a)'s
  handler does), `reset` pushes it and runs the machine.

- [x] the laws: `reset(pure(v)) == pure(v)`; `reset(shift(k => k(v)))
      == pure(v)`; `reset(shift(_ => pure(v)))` aborts.
- [x] D-F's examples, no other effect: `reset(shift(k => k(1) + k(10))
      * 2) == 22`.
- [x] the same with `State` after the capture: `k` runs the rest twice,
      the state threaded through both.
- [x] multi-shot with `Choose` handled outside: every branch, in order.
- [x] nested resets, different answer types, each body in its own row.
- [x] nested resets, the same answer type: the innermost answers.
- [x] direct style: the body of `shift` and the block under `reset`
      as `direct` blocks.
- [x] depth: 100 000 captures in sequence. Nested `reset`s: between
      10 000 and 30 000 on the default stack (see Results).
- [x] level 2: `.cont` / `.!` round trip on the diagonal, and one
      answer-type-modifying `Cont` with an effect in its answer.
- [x] the JMH lanes: (a) vs (b) vs today's `Cont` and `Delim` on the
      same shape.

- [x] round 2 (shift-effect-typed): Danvy-Filinski's `shift` (the body
      under its reset, `R ! Shift % R + F`) beside `shift0`; two shifts in
      sequence (55; 75 with State); a capture from inside another's body
      to the same reset (32).
- [x] answer types told apart: a compile-time key per answer type,
      `Shift % Int + Shift % String` in one row, a capture crossing a
      reset of another type (22); an abstract answer type refused.
- [x] direct style for `reset` and `shift` (`ShiftDirect`): the block
      under `reset` and the body of `shift` are `direct` blocks, and
      inside a block `shift` answers the value itself.

## Decisions

- **The body is `R ! F`.** Without `Shift % R` in the body's row, `shift`
  and `shift0` agree, and (a) pays one `reset` per capture. A body
  typed `R ! Shift % R + F` would need `reset(f(k))` in the clause, and
  then every capture re-walks what `k` already handled: quadratic in a
  sequence of captures.
- **`cont` is `reset(q >>= k)`**, not a fold of its own: it works on
  both implementations. A fold over `Shift` nodes was (a)-only. On (b)
  the nodes are Delim's, so it failed with a ClassCastException.
- **(b) shares ONE prompt** among all resets. The innermost installed
  one answers, as (a)'s handler does, and the row typing (Distinct)
  keeps a capture from reaching a `reset` of another answer type.

- **A key per answer type, made at compile time** (`Key[R]`, a macro
  in okay-direct's main, package-private): the normalised type, so an
  alias and a union in either order give one key. `Shift`'s `TypeableK`
  compares keys and is `ByValue`, so `Distinct` passes two answer types
  in one row. An abstract `R` has no key and is asked for one, as a
  `ClassTag` is. The macros sit in main only because zinc fails on a
  macro expanded in the run that defines it ("Failed to find name
  hashes").
- **(b) nests on one machine when the type says so** (`Nesting[F]`, read
  off the row): a `reset` whose row still holds a `Shift` only pushes its
  prompt, and the outer `reset` runs the machine. A `reset` whose row has
  none runs its own, so nested resets of the SAME type, each a complete
  program, still start a machine each.
- **Direct style needs no new macro.** `reset` wraps its block in
  `direct`; `shift` is `transparent inline`, a mark inside a block
  (`summonFrom` on `DirectCtx`, as `tell`) and the program outside.

- **Level 1 takes the names** (operator, 2026-10-02, "Да. Да. Делай"):
  `shift`/`reset` at the top level become `Shift % R`'s. Cont's move to
  `Cont.shift`/`Cont.reset` (cont-shift-rename) with no change in
  behaviour. Overloading was not an option: both take a lambda `k => …`,
  and a lambda with no parameter types cannot pick an overload.

- **In the core (shift-effect-core): (b) only.** `src/main/scala/Shift.scala`:
  top-level `shift` (D-F), `shift0`, `reset`; `Shift` is a phantom (its
  operations are Delim's `Cont0`), its `TypeableK.ByValue` compares the
  prompt. `Shift.Key` interns one key per normalised type id in a
  `ConcurrentHashMap` and holds that key's prompt, so a `shift` costs one
  map read, not a TrieMap update. `Shift.Nesting` counts `Delim` as well
  as `Shift`, so a `reset` inside Delim's own `delimited` pushes on that
  machine. `Shift.cont`/`Shift.embed` are level 2's doors. The probe's
  (a) and its suite are gone. Their numbers stay in Results.
- **Direct style needs nothing of its own** (shift-effect-core): `reset(direct
  { … })`, `shift(k => direct { … })`, with `.?` or with auto-colouring
  (`Free.directColor`). The probe's `ShiftDirect` is gone.

- **One `handle`, its handlers values** (handle-handler-values,
  operator's "Да"): `trait Handling[E, I, O, Needs]` (I bounds the
  answer, O shapes the result, Needs is what the handler needs of the
  rest of the row; `Handling.Plain` needs nothing), and `p.handle(h)` in
  `Freer`'s companion. The rest of the row is inferred by the `=:=`
  evidence between the program's row and `E + F`, which unifies the
  union as the effects' own runners do. A path-dependent `h.Needs[F]`
  failed: the argument `State(5)` has no stable path, so `Needs` is a
  type parameter. Ready: `State(s)`, `Reader(r)`, `Writer.log`,
  `Throws.either`, `Choose.all`, `Maybe.option`, `Reset[R]`; `p.run` for
  a program with no effect left. The `A throws E` extension that held
  the name `handle` at the package level moved into `object throws`,
  its type's companion, where its siblings already were. The old
  runners stay.

- **The typeclass door** (effects-shift-reset): `Effects[M]` gains `shift`,
  `shift0`, `reset`, `handle(m, h)` and `run(m)`. Their default goes
  through the tree (`reify`, the top-level function, `reflect`), so every
  encoding has them. `Effects[Free]` overrides them with the top-level
  functions themselves, so there is one definition. `handle` takes its
  program and handler in ONE list: the level-2 `handle(m)(ret)(clause)`,
  used 70 times, is an overload, and `E.handle(p)(State(5))` resolved to
  it and failed. `Effects.monad[M, F]` makes `M[F, *]` a `Monad` for
  `direct` in generic code. One program written over `Effects[M]`, in
  both styles, answers the same in `Free` and in `Eager` (TestEffectsLevel1,
  TestEffectsLevel1Direct).

## Results

Probe: `okay-direct/src/test/scala/ShiftFx.scala`,
`TestShiftFx.scala` (13 tests × 2 implementations, green),
`okay-direct/src/jmh/scala/okay/ShiftFxBenchmark.scala`.

**It types without annotations beyond the effect's own.** The user writes
`Int ! Shift % Int + State % Int`, `shift[Int, Int, S](k => …)` and
`reset[Int, S](q)`. Direct style needs no new macro: the body of `shift`
is a `direct` block with `k(1).?`.

**JMH** (one lane per run, box quiet throughout, history.d
`shift-effect-probe`):

| lane | (a) handler | (b) Delim machine | Cont today | Delim today |
|---|---|---|---|---|
| `seq`, 1000 captures, one reset | 86.9 µs, 894 KB | **54.8 µs, 534 KB** | 60.8 µs, 518 KB | 53.9 µs, 534 KB |
| `twoShot`, 100 resets of `k(1) + k(10)` | **7.29 µs**, 77 KB | 9.53 µs, 75 KB | 7.92 µs, 68 KB | 10.50 µs, 78 KB |

- (b) costs exactly what Delim costs: the API adds nothing. Its
  price is the machine's start per `reset`, visible in `twoShot`.
- (a) loses 1.59x on a sequence: every capture goes through
  `Effects.handle`'s capture arm, a `Cont` per capture plus a `Delay`.
  It wins small resets, which start no machine.
- Cont today sits between the two and is not faster than (b) on the
  sequence.

**The nesting limit is not Shift's.** Each `reset` runs its own handler
(machine) inside its parent's, so JVM depth grows with nesting. Both
implementations pass 10 000 and fail by 30 000. Any nested handler
does the same, and Delim's own rule is "one `Delim.run` per program"
(`scope` is its nested form). On (b) it has a direct fix: a `reset`
inside a running machine should push its prompt on that machine instead
of starting a second one.

**Round 2** (history.d `shift-effect-typed`):

| lane | (a) handler | (b) Delim machine |
|---|---|---|
| `seq` with keys | 91.6 µs, 902 KB (1.05x) | 60.6 µs, 534 KB (1.11x: the prompt looked up by key per `shift`) |
| `seqDF`, D-F `shift` | no number: a `reset` per capture nests a handler per capture | 75.3 µs, 638 KB (+104 B a capture: the re-pushed prompt) |

Depth, D-F captures in sequence: (b) 100 000; (a) 1 000, overflows by
10 000. Nested same-type resets: 3 000 on both in this run (10 000 in the
first), so the ceiling moves with the JIT.

**In the core** (history.d `shift-effect-core`): `shift0` on `seq` 56.1 µs,
534 KB, against Delim's 55.0 µs (1.02x: the interned key's prompt costs a
map read). D-F `shift` on `seq` 69.1 µs, 638 KB. `twoShot` 10.3 µs.

**Recommendation:** (b). It matches Delim on the common shape and
already has the single machine that removes the nesting limit. Open:
(1) a `reset` inside a running machine only pushes its prompt; (2) the
machine's start cost for a small `reset` (`twoShot`, 1.31x behind (a)).
