# Single-shot row: the evidence that would licence a mutable accumulator

## Overview

Under the fused pass over `Free` (~122 B and ~14 ns per operation,
specs/handler-fusion.md) the only road left is to stop THREADING the
accumulator as a value and mutate a cell instead. That is sound only
when no continuation the runner hands out is ever resumed twice: a
multi-shot handler (`Choice.run`, `Logic`, the `List`/`Vector`
MonadPlus) re-runs a captured continuation, and a continuation that
closed over a mutable cell would see the SECOND run's writes on the
first run's cell. `State.handle`'s comment says exactly this, and it is
why every runner in the tree threads values.

This spec designs the evidence — "everything left in this row will be
handled single-shot" — and, before any API, PRICES what a mutable
accumulator would buy, because the arithmetic says: little. It is
written in the operator's order (item 1 of four, after 2, 4, 3), and
the measurement is the gate.

## What the evidence is about — and what it cannot be

Multi-shot is a property of a HANDLER, not of a signature. `Choose`
is multi-shot because `Choice.run` resumes `k` once per alternative;
a user can write `Effects.handle(p)(ret)([X] => e => shift(k => k(1) ++ k(2)))`
over ANY signature. So the evidence cannot be "this signature is
single-shot" as a fact; it is a PROMISE about the handlers that will
be applied to the residual row after the mutable runner — the
handlers OUTSIDE it, because those are the ones that receive the
residual's continuations and could call them twice.

```scala
/** the caller's promise: every operation of F left in the residual
 * will be handled by a handler that resumes each continuation at
 * most once. Library givens exist for the signatures whose library
 * handlers keep that (Reader, Writer, State, Throws, Async); none
 * for Choose, Logic, List, Vector. A ROW is single-shot when its
 * parts are. */
@implicitNotFound("no SingleShot[${F}]: a handler of ${F} may resume a continuation more than once, so a runner may not mutate.")
trait SingleShot[F[_]]
object SingleShot:
  given SingleShot[Pure] = new SingleShot[Pure] {}
  given [S]: SingleShot[State % S] = …
  given [W]: SingleShot[Writer % W] = …
  given [R]: SingleShot[Reader % R] = …
  given [E]: SingleShot[Throws % E] = …        // abort resumes zero times
  given SingleShot[Async] = …                  // once, by the callback contract
  given [F[+_], G[+_]](using SingleShot[F], SingleShot[G]): SingleShot[F + G] = …
```

**The hole, stated:** a user-written multi-shot handler over a
signature that HAS a library given (say a `Reader` handler that
resumes twice to explore two environments) breaks the promise and no
type catches it. `relay` enforces exactly-once by parametricity (an
answer-polymorphic handler cannot call `k` twice usefully); the
general `Effects.handle` hands out a plain `k`, and nothing stops a
second call. So `SingleShot` is a contract, like `Eager`'s stated
hazard and unlike `relay`'s — and a runner that consumes it must say
so in its name (`State.handleMut`, `Writer.runMut`), never be the
default.

## What it would buy — the arithmetic, before the measurement

- `State.handle`: the accumulator is ONE value already threaded as a
  loop parameter. A cell saves nothing per operation; the forwarded
  ops' closures capture `s` either way. Expected: ~0.
- `Writer.run` (after either-scalarised: a `List` prepended, reversed
  once): 24 B per tell for the cons + 24 for the reverse = 48. A
  `ListBuffer` appends in order (no reverse) at ~24 B per tell.
  Expected: ~−24 B per tell — on the 1 000-tell Writer-only program
  (128 024 B/op) about −19%; on the mixed program (333 tells,
  148 696) about −5%.
- `Fused.stateWriter` (the fused probe): the `((S, Vector[W]), A)`
  tuples are built once at the end; the per-op cost is the program's
  own nodes. Expected: ~−24 B per tell again, nothing else.

So the evidence's whole yield is Writer's reverse — a fifth of a
Writer-only pass, a twentieth of a mixed one — for a new typeclass and
a contract the types cannot enforce.

## Behavior

- [ ] PRICED FIRST (the gate): `Writer.runMut` as a private probe
      (ListBuffer under `SingleShot[F]`), same-run A/B against
      `Writer.run` on the Writer-only and the mixed lanes
      (`SplitBenchmark`). If the mixed-program saving is under 10%
      B/op, the evidence is NOT shipped: the probe and the numbers go
      into Results and the item closes.
- [ ] If shipped: `SingleShot` givens for the five signatures and the
      row; `Choice`/`Logic`/`List`/`Vector` refuse at compile time
      (`TestErrorMessages` pins the message); `runMut` agrees with
      `run` on generated programs; a multi-shot handler over a
      `SingleShot` row is DEMONSTRATED to go wrong in a test that
      exists to document the hole, not to pass silently.

## Out of scope

- Mutating anything a continuation can observe more than once.
- Enforcing single-shot by type for user handlers (would need a
  linear continuation type; not this library's `k`).

## Decisions

- **Price before API** — chosen because the arithmetic above puts the
  yield at Writer's reverse and nothing else; an evidence type that
  buys 5% on a mixed program is not worth its contract hazard.
  Rejected: shipping the givens first and measuring after (the
  handler-fusion arc's own lesson).
- **A contract, named `Mut`, never the default** — if shipped, the
  mutable runner is opt-in by name and its doc names the hole.

## Results

(after the probe)
