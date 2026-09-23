## scoped-effects-laws - `recover`×`State`'s order pinned; `Reader.local` built, with two limits found by testing rather than assumed

Wu-Schrijvers-Hinze's question ("Effect handlers in scope", Haskell
2014) — does an effect performed inside a scoped operation's body
survive the scope, or roll back with it? — was answered implicitly
(`Throws.recover` existed, untested against `State`) and left
unstated. Now stated, tested, and `Reader.local` fills the gap it
exposed.

- `recover`×`State`: GLOBAL by default (recover forwards State
  unchanged, so a write before a caught raise sticks and the recovery
  handler sees the state the failed attempt left, not a snapshot);
  SCOPED/transactional by running `State` inside the guarded block
  instead of outside it. Both pinned, plus the explicit save/restore
  idiom for a transactional retry over global state.
- `Reader.local[R, A, F](f)(p)` (src/main/scala/Reader.scala): built
  on `Effects[Free].handle`, `recover`'s own tool. The GADT refinement
  `Ask(): Reader[R,R]` moved into a private method — the SAME trap
  `SharedOnce.answer` met (a `[X] => ...` polymorphic lambda cannot
  refine a GADT the way an ordinary method's match can).
- TWO LIMITS, found by running tests that DISAGREED with the first
  draft's assumptions and were believed over the draft: (1) nesting
  `local(f2)(local(f1)(p))` composes INSIDE-OUT — `f1(f2(r))`, not
  mtl's `f2(f1(r))` — because `local`'s own "find r" step is an
  ordinary, unprotected `ask`, reachable by an enclosing `local`
  exactly like a user's own; (2) `local`/`recover` cannot reach inside
  another effect's OPAQUE operation payload — demonstrated against
  `Delim.push`'s `body` field, a plainer and more basic limit than the
  literature's "delimited dynamic binding" (Kiselyov, Shan & Sabry,
  ICFP 2006), which the spec's first draft wrongly claimed did not
  apply until a test proved otherwise.
- TestScopedEffects: 11 tests, including the documented trap
  (a Reader program value carries no lexical memory of where it was
  built) demonstrated without needing Delim at all.
- docs/guide.md: the scoped-effects paragraph, both examples pinned
  verbatim in a gated test. specs/scoped-effects-laws.md carries every
  finding, including the two the first draft got wrong.
