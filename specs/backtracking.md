# Backtracking — LogicT over Choose

## Overview
Logic-programming search as a LIBRARY over the existing
nondeterminism effect (Kiselyov–Shan–Friedman–Sabry 2005, "Backtracking,
interleaving, and terminating monad transformers"): no new effect, no
new handler capability — Choose was already multi-shot, and the whole
of LogicT derives from ONE primitive over it.

## Design
- `Logic.msplit(m): Option[(A, rest)] ! F` — the primitive: the first
  answer and the REST OF THE SEARCH as a program (or None: empty
  search). Depth-first, left to right; a worklist walk over the freer
  tree, alternatives split by `<|>`, F-operations forwarded and run
  once, when the search first crosses them. The worklist is a
  LazyList and `Choose.as` is a `Seq` — a `LazyList` of alternatives
  makes an INFINITE choice point that costs nothing to construct.
- Derived, exactly as in the paper:
  - `cut` — commit to the first answer, drop the rest (`once` until logic-cut, 2026-09-16);
  - `ifte(c)(th)(el)` — the SOFT cut: `th` over ALL answers of `c`,
    `el` only when `c` has none (a plain flatMap cannot say "no
    answer"; a hard cut would lose the other answers);
  - `gnot` — negation as failure (one line over ifte);
  - `interleave` — the fair or: two infinite branches take turns;
  - `fairBind` / `>>-` — the fair bind: a productive branch cannot
    starve its siblings;
  - `observe(n)` — the first n answers of a possibly infinite search.
- `guard` (MonadPlus, in Monad.scala) is the pruning conditional the
  searches read naturally: `guard(p).map(_ => x)` keeps the branch
  exactly when p.

## Behavior
- [x] pythagorean triples by choose + guard, in generation order
- [x] msplit returns the first answer and a runnable rest; None on
      an empty search
- [x] once keeps exactly one answer; once of empty is empty
- [x] ifte: then over ALL condition answers; else ONLY on no answer;
      gnot succeeds exactly on failure
- [x] interleave of two infinite streams takes strict turns (six
      answers = 0..5 of evens⋈odds)
- [x] fairBind finds a witness under an infinite generator where the
      unfair bind diverges
- [x] observe(n) of an infinite search terminates lazily
- [x] F-effects forward: a Writer told on the crossed path is told
      once per crossing, in search order

## Decisions
- **Library, not effect**: Choose's multi-shot handler was already
  the hard part; LogicT is an eliminator vocabulary over it. No new
  signature means every existing instance (MonadPlus[A ! Choose],
  runChoice) composes unchanged.
- **The laziness contract bit back during construction**: the first
  interleave/fairBind recursed AT BUILD TIME (eager argument
  evaluation), and `as.toList` forced infinite alternative streams —
  exactly the eagerness this library's own doctrine forbids. Fixed by
  the standard `pure(()).flatMap(_ => …)` deferral, a by-name second
  argument, and a LazyList worklist. The lesson is recorded because
  it is the SAME bug the compare suite catches kyo on.
- msplit walked eagerly when CALLED up to the first answer or
  F-operation, until handle-frames-catch (2026-10-03): it is a VALUE
  now, its search run when the program is — as a depth-bounded fold,
  or as a search frame on a machine (specs/handle-frames.md, "Catch
  frames, Resource, the search"), so searches nested 100 000 deep
  hold a bounded host stack.

## logic-named-cut (filed, GATED on a search consumer)

`cut`/`ifte` are the local cuts and cover the practical cases;
Prolog's NON-LOCAL cut — committing through several choice points
to a NAMED barrier — is abort-to-prompt, i.e. Delim over the Logic
row (the doctrine's cross-boundary case). Deliberately gated: no
search consumer needs it yet, and machinery for a need nobody named
is this repo's named anti-pattern. The gate lifts when a
planner/solver consumer exists (agent-search is the likely one).

## Out of scope
- committed-choice/pruning beyond once (cut scopes), tabling,
  unification — a Prolog is a user of this, not this

## A resource shared by branches (logic-cut-releases, 2026-10-04)

MEASURED FIRST (a probe on master): the entry as filed was wrong — `cut`
over a scope leaks nothing, because the answer `msplit` returns has passed
the end of every scope on its path. The real defect is the other way
round: a resource acquired BEFORE a `choose` is acquired once and released
once PER BRANCH — `runChoice` over three alternatives released it three
times. The operator chose (2026-10-04): the resource is SHARED by the
branches, as every effect of a search's prefix is ("run once, when
crossed"), and released ONCE, when the last branch that can use it is
done.

THE MECHANISM, a reference count per acquisition:
- a scope's acquisition is a `Held` with one holder, the path that took it;
- a `Choose` passing OUT of a scope that holds acquisitions goes on with
  its alternatives wrapped (`Forked`, still a `Seq`, still `Choose`): the
  path's holder moves to the branch point (`Fork`), and every branch the
  handler starts — the scope's continuation called — is one holder more;
- a branch ends (its value through the scope, a throw, a final operation,
  a discontinue): one holder fewer; the release runs at zero;
- the branch point is done when every alternative has started (counted
  against `knownSize`, or the alternatives run out for a lazy list), or
  when it is ABANDONED: a handler that will start no more of them says so.

WHO ABANDONS:
- `Logic.cut` and `observe` drop the rest of a search: they abandon it
  (`Logic.abandon(rest)`; `msplit`'s rest carries the branch points it
  still holds, and a split of a rest inherits them);
- a throw that leaves a search (`runChoice`, `msplit`) abandons the branch
  points it holds: no handler will resume them;
- a third-party handler that stops early: `Choose.abandon(c)`. One that
  stops early without saying so keeps the resource open — the same
  contract as a dropped continuation (`Shift.discontinue`).

Behaviour:
- [x] every branch: the shared resource released once, after the last branch
- [x] `cut`: released once
- [x] `observe(n)` of more: released once; of an infinite choice: released once
- [x] a scope inside a branch: its own resource released once per branch, the shared one once
- [x] a throw out of the search: released once
- [x] on a machine (the frames): the same
- [x] a scope with nothing held when the choice passes: no wrapping, unchanged cost

Results (TestSharedResource, cross, JVM and JS; red first on the seven that
changed — three releases for three branches):
- COST (history.d logic-cut-releases): `runChoice` with no scope 0.99x;
  `observe` with no scope 1.02x (the branch points list `msplit` carries,
  empty); 100 branches over one shared acquisition 1.11x, ~2.4 us — the
  holders, the branch point and the guard that abandons on a throw,
  against a ref that released the resource 100 times.
- NOT COUNTED: an alternative a handler takes and never starts without
  saying so (a filter of its own): the branch point waits for it, and the
  resource stays open — the contract `Choose.abandon` is for.

