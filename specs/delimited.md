# `Delimited`: the continuation machine behind an interface

Operator ask, 2026-10-01: separate the machine from the operations it
runs, the way Dybvig, Peyton Jones and Sabry separate theirs ("A
monadic framework for delimited continuations", JFP 17(6), 2007): a
TYPE CLASS of a few primitives, every control operator derived over it,
and the machine one implementation of the class. Our variant, not
theirs (operator, the same day): the capture is `shift0` — its `k`
keeps the delimiter and the delimiter's `ret`.

## Overview

DPJS's class:

```haskell
class Monad m => MonadDelimitedCont p s m | m -> p s where
  newPrompt   :: m (p a)
  pushPrompt  :: p a -> m a -> m a
  withSubCont :: p b -> (s a b -> m b) -> m a
  pushSubCont :: s a b -> m a -> m b
```

Ours, `trait Delimited[M[_, _, _]]` over a carrier `M[T, R, A]` (the
freer tree's indexes: the program goes from answer `R` to `T` and
produces `A`):

| ours | DPJS | what it is |
|---|---|---|
| `type Delimiter[Y, I]` | `p a` | a delimiter's name: answer `Y`, installed at index `I` |
| `type SubCont[A, S, T, Z] <: A => M[S, T, Z]` | `s a b` | a captured stack, from `A` to the delimiter's answer `Z` |
| `delimiter[Y, I]` | `newPrompt` | a fresh name (not a program: identity is allocation) |
| `dollar(d)(ret)(body)` | `pushPrompt` | `ret $ body` (λ$); `pushPrompt` is `pure $` |
| `shift0(d)(f)` | `withSubCont` | capture to `d`: `k` WITH the delimiter and its `ret`, the body in the delimiter's place |
| `resume(k)(m)` | `pushSubCont` | run the COMPUTATION `m` inside `k` — `k(a)` is `resume(k)(pure(a))` |
| `pure(a)` | `return` | a value |

Two differences from DPJS, both decided:

- **`shift0`, not `withSubCont`.** DPJS's capture leaves the prompt out
  of `k` (it is `control0`), and `shift0` is derived by re-pushing it.
  With `$` the delimiter carries `ret`, which the derived form would
  have to hand back beside `k`; ours keeps it in `k` — λ$'s `($/S0)`
  rule, the machine as it is (specs/cont-core.md).
- **`$`, not only `pushPrompt`.** A deep handler's return clause runs
  OUTSIDE the delimiter and rides in `k`; `pushPrompt p (m >>= ret)`
  runs it inside. `reset` is `dollar(d)(pure)`.

## Derived, over any instance

| operator | as |
|---|---|
| `reset(d)(body)` | `dollar(d)(pure)(body)` |
| `shift(d)(f)` | `shift0(d)(k => reset(d)(f(k)))` — `S k.e = S0 k.⟨e⟩` |
| `abort(d)(v)` | `shift0(d)(_ => pure(v))` |

## The machine as an instance

`Delimited.machine[F]`: `M[T, R, A] = Freer[Cont0.Row[F], T, R, A]`,
`Delimiter = Cont0.Delimiter`, `SubCont = Stack[F, ...]`. The
primitives are the operations the frame machine already runs:
`dollar` is `Inject(Dollar0)`, `shift0` is `Inject(Shift0)`, and
`resume(k)(m)` is `Bind(m, k)` — a `Bind` whose continuation is a
`Stack` is the machine's resumption rule, so resuming with a whole
computation needs no new operation. One stateless object serves every
`F` (`noFrames`'s pattern: the cast is that sentence).

`Cont0`'s companion keeps the DATA (the two operations, the row, the
delimiter type, `prompt`, `boundary`); the derived operators move to
`Delimited`, and the clients — `Delim`'s doors, `Lexical`, `TestKont`,
`KontBenchmark` — call the instance.

## Stages

1. **The trait, the machine instance, the clients** (this lane).
2. **A second instance, the reference** (`Control[Func]`'s role for
   `Control`): a plain recursive interpreter of the same four
   primitives, not stack-safe, used to check the machine's answers on
   the suites' programs — the abstraction earns its keep when two
   implementations agree.
3. **Resume with a computation, used**: `resume(k)(m)` with `m` not a
   value — "throw into a continuation" (DPJS §4) — as a test and a
   doc example.

## Behavior

- [x] `trait Delimited[M]` with `Delimiter`, `SubCont`, `delimiter`, `pure`, `bind`, `run`, `dollar`, `shift0`, `resume`; `reset`, `shift`, `abort` derived in it
- [x] `Delimited.machine[F]` is the frame machine; one object for every `F`
- [x] `Cont0`'s derived helpers gone; `Delim`, `Lexical`, `TestKont`, `KontBenchmark` call the instance
- [x] `resume(k)(m)` with a computation: a test (stage 3)
- [x] the 24 continuation suites green; `affected master staged` green
- [x] no lane slower than master by more than noise (an interface over the same nodes: no new allocation, no new step)

- [x] ONE DOOR (delimited-one-door, operator: "Одна дверь в машину"): `runHead` (run to head form) in the trait, `run` is it under a boundary; `Frames.run`/`enterAt`/`uncat` private and `Own`'s constructor closed (`Delimited.Machine` lives in `object Frames`, beside the loop); `Cont`'s bridge (`runHead(k(x))`, `retOf` for its root), `Shift`'s nested runs and `Stacked` (`runHead`, `owned`), `TestKont` and `KontBenchmark` through the interface
- [x] `runHeadAt(k)(a)` = `runHead(k(a))` without the resumption node, the strict-`k` bridge's road; statePara at master's bytes (549 336 B) and time (61.5 vs 63.3 us) after two refuted rounds (history.d `delimited-one-door`)
- [x] the reference's `runHead` is the program itself; TestDelimitedDifferential runs sub-programs through it at random points (`Prog.RunHead`), captures crossing it included, on every platform; a mutant `runHead` with a boundary (a crossing capture made `NoPrompt`) fails all three program sets
- [x] `Delimited[M]` extends `ParaMonad[[A, S, R] =>> M[S, R, A]]`: `pure[A, R]` in Atkey's order, `flatMap` = `bind`

## Decisions

- Name `Delimited` (operator). Primitive names are λ$'s (`dollar`,
  `shift0`) and the machine's (`resume`), DPJS's beside them in the
  table, because our `shift0` is not their `withSubCont`.
- `delimiter` is a plain function, not a program: a prompt's identity
  is its allocation (`eq`), so making one needs no effect.

## Literature

- R. Kent Dybvig, Simon Peyton Jones, Amr Sabry. *A monadic framework
  for delimited continuations.* JFP 17(6), 2007.
- Marek Materzok, Dariusz Biernacki. *A dynamic interpretation of the
  CPS hierarchy.* APLAS 2012 (λ$).

## Results

- Stage 1 (0c78b00c8): the trait, the machine instance, the clients.
  The trait gained `bind` and `run` in stage 2 (DPJS's class is a monad
  and has `runCC`).
- Stage 2 (45a5b4d69): `DelimitedReference` in the tests — a list
  context, ~40 lines, not stack-safe; TestDelimited's seven laws are
  written against `Delimited[M]` alone and agree on both instances.
- Stage 3 (985bdb4ef, 45a5b4d69): resuming with a computation needs no
  operation — `Bind(m, k)` is the machine's resumption rule. An abort
  to a delimiter only `k` carries, and a capture to it resumed twice,
  both answer inside `k`.
- A/B against master (history.d `delimited-trait`): delimPushOnly,
  delimGenerator, contAnswer, statePara 0.98-1.01x at identical bytes.

