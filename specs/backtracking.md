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
  - `once` — the cut: commit to the first answer, drop the rest;
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
- msplit (like every handler here) walks eagerly when CALLED up to
  the first answer or F-operation — eliminators run, programs don't.

## logic-named-cut (filed, GATED on a search consumer)

`once`/`ifte` are the local cuts and cover the practical cases;
Prolog's NON-LOCAL cut — committing through several choice points
to a NAMED barrier — is abort-to-prompt, i.e. Delim over the Logic
row (the doctrine's cross-boundary case). Deliberately gated: no
search consumer needs it yet, and machinery for a need nobody named
is this repo's named anti-pattern. The gate lifts when a
planner/solver consumer exists (agent-search is the likely one).

## Out of scope
- committed-choice/pruning beyond once (cut scopes), tabling,
  unification — a Prolog is a user of this, not this
