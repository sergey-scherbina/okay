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
case Suspend[A, S, R](thunk: () => Cont[A, S, R]) extends Cont[A, S, R]
```

and `Eff.flatMap` becomes

```scala
[S] => h => Cont.Suspend(() => m[S](h)).flatMap(a => f(a)[S](h))
```

so that applying a program to a handler returns at once (a `Suspend`
under a `Bind`), and the inward applications happen inside `/`'s
loop, one per iteration, with `Bind(Bind(Suspend, f), g)` rotated
exactly as `Bind(Bind(a, f), g)` is today.

## Behavior

- [ ] a left-nested chain of 1 000 000 `Eff` binds runs on the default
      test stack (TestEffects), and a right-nested one still does.
- [ ] `Free` and `Eff` agree on both shapes (`fromFree[Eff]` of the
      Free chain answers what the Free chain answers).
- [ ] `Cont`'s own laws are unchanged: the `Control[Cont]` suite and
      every existing test green; `Suspend` never reaches user code (no
      public constructor of it is needed; `flatMap`/`map` on a
      `Suspend` build a `Bind`, as on any non-`Shift`).
- [ ] MEASURED: `FusionBenchmark.effSWr` (right-nested, 23.5 µs /
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
