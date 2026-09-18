- proc-notation — the `direct` macro at an arrow (specs/proc-notation.md,
  stage 0 the spec LANDED 2026-09-18). WHY: static-workflow's first
  price line was "combinators, not straight-line code"; the operator
  ruled it is designed away before stage 1 ships. WHAT: `Proc.direct`,
  the SAME block text as the `Wf` booking compiled to a `Proc` — the
  macro threads the environment in `Arr`s (never journalled), `if` →
  `Left`, `while`/`for` → `Iter`, a leaf chosen by a bound value (=
  `app`) refused by name. HOW: a third road in Direct.scala beside
  `compileAll` and `applicativeOnly`, reusing `asMark`/`hasMark`/
  `mentionsAny`/`independentRun`; the IR refactor (stage 2) only if the
  third road duplicates more than the helpers. DONE-WHEN stage 1: the
  booking text compiles at both entries from ONE test source and the
  two journals are byte-equal; the `app` refusal is compileErrors-
  pinned; every existing direct test unchanged in bytes.
