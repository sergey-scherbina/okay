## cont-list-combinators-one-walk — Cont's collection lowerings over one walk (2026-10-03)

Commits: 3451d69d4.

- Cont.scala's `traverse`, `foldIn`, `existsIn` and `findIn` — what
  ContMacro lowers `map`/`foreach`, `foldLeft`, `exists`/`forall`
  and `find` to inside an answer-using shift body — had four private
  recursive helpers of one shape. They are one private `walk` now: per
  element a program over the lazy `k`, whose answer stops the walk or
  goes on; recursion deferred to the machine as before.
- The caller picks the sequence, which keeps what each had: a strict
  `List` where every element is visited, the memoised `LazyList`
  cont-stack-layer1-c gave `exists`/`find` (an early stop forces
  nothing after it, an infinite receiver included). Going on is a null,
  not an `Either`, so nothing is allocated per element that was not
  before.
- Fewer lines only modestly (27 in, 29 out, the walk's doc included);
  the gain is one walker to reason about instead of four.
- Guarded: a mutant reversing `map`'s order reds two TestContMacro
  tests (order, and flatMap/exists/find).
- Answered on the way: `handle-overloads-chain` — keep the 2- and
  3-handler `handle` overloads (public API, and the okay2 twin has and
  documents the same forms).

Gate: `affected master staged`, GREEN, 9 701 tests, no warnings.
