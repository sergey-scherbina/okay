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
- (b) the same API on `Delim`'s machine: `shift` is `Delim.shift` to
  one shared prompt (the innermost installed one answers, as (a)'s
  handler does), `reset` pushes it and runs the machine.

- [ ] the laws: `reset(pure(v)) == pure(v)`; `reset(shift(k => k(v)))
      == pure(v)`; `reset(shift(_ => pure(v)))` aborts.
- [ ] D-F's examples, no other effect: `reset(shift(k => k(1) + k(10))
      * 2) == 22`.
- [ ] the same with `State` after the capture: `k` runs the rest twice,
      the state threaded through both.
- [ ] multi-shot with `Choose` handled outside: every branch, in order.
- [ ] nested resets, different answer types, each body in its own row.
- [ ] nested resets, the same answer type: the innermost answers.
- [ ] direct style: the body of `shift` and the block under `reset`
      as `direct` blocks.
- [ ] depth: 100 000 captures in sequence; 100 000 nested `reset`s.
- [ ] level 2: `.cont` / `.!` round trip on the diagonal, and one
      answer-type-modifying `Cont` with an effect in its answer.
- [ ] the JMH lanes: (a) vs (b) vs today's `Cont` and `Delim` on the
      same shape.

## Decisions

## Results
