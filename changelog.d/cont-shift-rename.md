## cont-shift-rename - Cont's shift/reset are Cont.shift/Cont.reset

Level 1 of the API, step 1 (operator, 2026-10-02; specs/shift-effect.md).
The top-level `okay.shift`/`okay.reset` were Cont's, and level 1 needs the
names for `Shift % R`, the continuation as an effect.

- `Cont.reset` is new beside `Cont.shift`, and the top-level pair is gone.
  Behaviour is unchanged.
- Every call site moved: handler clauses (`Cont.shift(k => …)`),
  Monadic.reflect, the codecs' `Cont.reset`, tests, benchmarks. Each was
  found by the compiler, not by a text search, because `Delim.shift` and
  others share the name. Imports `okay.{…, reset}` became `okay.{…, Cont}`.
- The docs that quote them follow (TestDocSnippets), including seven debt
  lines. One debt line went, its source line paid.
- okay2 is its own build and keeps its names.
