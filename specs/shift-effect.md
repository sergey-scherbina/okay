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

**Recommendation:** (b). It matches Delim on the common shape and
already has the single machine that removes the nesting limit. Open:
(1) a `reset` inside a running machine only pushes its prompt; (2) the
machine's start cost for a small `reset` (`twoShot`, 1.31x behind (a)).
