# Eff stack safety: a left-nested bind must not recurse before a Cont exists

## Overview

`Eff[F, A] = [S] => F !> S => A /> S` is the Church encoding: a program
is the function of its handler. Measured 2026-09-09 (eff-stack-safety,
a probe in a scratch test, default test JVM stack): a RIGHT-nested
chain of one million `Eff` binds runs; a LEFT-nested chain
(`foldLeft`-built, `((m flatMap f) flatMap g) …`) overflows somewhere
between 10 000 and 100 000. `Free` runs both at a million, and the
library says so everywhere ("the tree is for stack safety on any bind
shape; Eff is not stack-safe on a left-nested flatMap").

The reason is exact and it is not "closures are not stack-safe".
`Eff.flatMap(m)(f) = [S] => h => m[S](h).flatMap(a => f(a)[S](h))`.
Applying the outermost program to a handler CALLS the next inner
program's application, which calls the next, n frames deep, before
the first `Cont` node exists — each frame must still `.flatMap` the
value the inner call returns, so none is a tail call. The Cont that
comes back IS stack-safe data with a tail-recursive runner; the
overflow happens in the n nested applications that precede it.

## Interface

Nothing public changes shape. Added, in `Cont`:

```scala
/** a deferred computation: the runner unfolds it in its own loop */
case Defer[A, B, S, T, R](thunk: () => Cont[A, T, R], f: A => Cont[B, S, T]) extends Cont[B, S, R]
```

and `Eff.flatMap` becomes

```scala
[S] => h => Cont.defer(() => m[S](h))(a => f(a)[S](h))
```

so that applying a program to a handler returns at once (one `Defer`
node: a bind whose left side is a thunk), and the inward applications
happen inside `/`'s loop, one per iteration, with `Bind(Defer(t, f), g)`
rotated into `Defer(t, f andThen flatMap g)` exactly as `Bind(Bind(a, f), g)`
is today.

## Behavior

- [x] a left-nested chain of 1 000 000 `Eff` binds runs on the default
      test stack (TestEffects), and a right-nested one still does.
- [x] `Free` and `Eff` agree on both shapes (`fromFree[Eff]` of the
      Free chain answers what the Free chain answers).
- [x] `Cont`'s own laws are unchanged: the `Control[Cont]` suite and
      every existing test green; `Suspend` never reaches user code (no
      public constructor of it is needed; `flatMap`/`map` on a
      `Suspend` build a `Bind`, as on any non-`Shift`).
- [x] MEASURED: `FusionBenchmark.effSWr` (right-nested, 23.5 µs /
      297 897 B/op today) does not regress; a new left-nested `Eff`
      lane exists and runs. Prediction: right-nested moves LITTLE
      either way — a `Suspend` replaces nothing on the hot path but
      the Shift fusion no longer triggers across an Eff bind (the
      inner value is deferred), so the Bind path is taken; the Free
      loop measured that path FASTER than closures in stage B, so no
      regression is expected, and any is recorded.

## Out of scope

- Making `Eff` faster than `Free` (handler-fusion.md stage B closed
  that: 0.58x, the tree is cheaper than the closure pair).
- Any change to `Free`, `relay`, the runners, or `Control[Func]`
  (Func stays the fast, not-stack-safe carrier by design).

## Decisions

- **`Suspend` in `Cont`, not "reify to Free"** — chosen because the
  fix must not add a walk: `reify` rebuilds every node before running.
  A `Suspend` is one node per Eff bind, unfolded by the loop that
  already rotates `Bind`. Rejected: a `Shift(k => inner / k)` — the
  inner run executes INSIDE the outer shift's frame, so the nesting
  moves into the runner and stays n deep (checked by reading `/`:
  `Bind(Shift(s), f) => s(f(_)(k))` calls `s`, and `s` runs the inner).
- **Eff's type unchanged** — the alias stays `[S] => F !> S => A /> S`;
  only `flatMap`'s body defers. `pure` and `perform` stay eager (a
  `Cont.Pure` / the handler's answer), so a program with no binds costs
  what it did.

## Results

2026-09-09, FusionBenchmark, box at load 10–48, minima over 3 forks;
B/op from -prof gc, load-proof. Baselines are stage B's (23.5 µs /
297 897 B/op for `effSWr`).

| lane | before | Suspend node under a Bind | ONE Defer node |
|---|---|---|---|
| `effSWr` (right-nested, 1 000 ops) B/op | 297 897 | 345 888 (+48/bind) | **329 888 (+32/bind)** |
| `effSWr` µs, same-run `fusedSWr` as load reference | 23.5 (ref 13.7) | 30.2 (ref 17.2) | **29.2 (ref 14.9)** |
| `effSW` (LEFT-nested, 1 000 ops) | ran (1M did not) | 56.9 / 641 137 | **56.0 / 609 137** |
| a million left-nested binds | StackOverflowError | runs | **runs** |

The prediction "moves little" was wrong: deferring costs the fast
right-nested path the thunk and a node it did not have — the eager
path fused each bind into the previous `Shift`'s closure (56 B), the
deferred one is a `Defer` plus, at run, the `Bind(Shift, f)` closure
(≈80 B). Two shapes were measured and the single-node one kept: +11%
allocation, ~+14% time load-adjusted on `Eff`'s right-nested path.

**Decision, stated so it can be reversed in one line:** kept. `Eff`
is stack-safe on any bind shape from here, at that price. The
reasoning: the overflow was silent and shape-dependent (a
`foldLeft`-built program of 100 000 binds dies, a for-comprehension
of the same size runs), which is the kind of footgun this library
refuses elsewhere; `Free` already exists for anyone who needs the
last 11% AND stack safety; and stage B measured `Eff` at 0.58x of
the fused Free loop for stateful rows before this change, so the
"speed encoding" argument for keeping the tax off was already weak.
If a consumer's pipeline is right-nested and hot, the revert is
`Cont.defer(() => m[S](h))(…)` → `m[S](h).flatMap(…)`.

