# atm-beyond-state-docs — answer-type modification is not "the state trick"

## Overview

The operator asked, of `PState`: "это нужно и можно только для state?"
The docs let that question arise honestly — chapter 3's "Instance one"
(`Cont`'s answer-type modification) stated the abstract theory and then
every WORKED example in the chapter was state-shaped (`PState`,
`Stage.phased`, `Typed.region`). Answer-type modification is `shift`'s
OWN typing — Danvy & Filinski's original point — and state is its most
common tenant, not its only one. This lane adds the non-state
illustration the chapter was missing: Asai's typed `printf`, built
directly on `Cont`, no state cell anywhere.

## Interface

```scala
def lit(s: String): Cont[Unit, String, String] = shift(k => s + k(()))
def hole[T]: Cont[T, String, T => String] = shift(k => (t: T) => k(t))
```

`lit` performs no answer-type change (`S -> S`, ordinary sequencing);
`hole` is the whole mechanism — its `shift`'s continuation `k: T=>S`
is handed straight through as "the rest of the format, given a `T`",
so the WHOLE program's answer type gains an arrow, `T => S`, instead
of staying `S`. Composed via ordinary `Cont.flatMap`, no new
combinator, no new type.

## Behavior

- [x] one hole: `hole[Int].flatMap(n => lit(...))`'s answer type is
      `Int => String`, not `String` — and running it (`/`) with the
      "nothing more" continuation gives a genuine `Int => String`
      function, correct on more than one input
- [x] a literal alone needs `/`, not `reset` — its own `A` (`Unit`)
      is not its own `S` (`String`), so the diagonal `reset` (which
      needs `A = S`) does not apply to it, only the general `/`
- [x] REFUTED, and kept as a `compileErrors` pin rather than prose: a
      SECOND hole does not compose by nesting a further `flatMap`
      inside the first hole's continuation — `bind`'s own signature
      requires whatever comes next to answer EXACTLY the first
      shift's `S`, and a second hole answers `T => S` instead. The
      first draft of this lane assumed two holes chain the same way
      two literals do; they do not, and the assumption was corrected
      against the compiler rather than left standing.

## Out of scope

- **A general multi-argument typed printf.** Asai's own paper needed
  "three NEW solutions" for exactly this case — nested reset
  boundaries or a different encoding entirely — and rebuilding one of
  them is a research exercise, not a documentation illustration. One
  hole makes the pedagogical point (the answer type moves) completely;
  a reader who wants N holes has the paper's own citation.
- **A `Cont`-level combinator library for format strings** (a `%d`/
  `%s` DSL, a `Format[A]` type). Nothing in this codebase asks for
  one; `lit`/`hole` exist to illustrate the mechanism, not to become
  a feature.

## Decisions

- **The example lives in theory chapter 3, not a new module.** It
  belongs exactly where "Instance one" already introduces answer-type
  modification in the abstract — a worked non-state example there
  closes the gap the operator's question exposed, and a whole new doc
  page would separate the illustration from the theory it illustrates.
- **`Prog` (freer-base stage 2) is named as the theory's THIRD
  instance, added to "Instance two"'s discussion.** It carries the
  same Atkey index as `PState` but over `Free` rather than `Cont`,
  and the chapter's own "one trait, one theorem, N instances"
  framing was one instance short of the current codebase.

## Results

Landed as written: theory chapter 3 gained the printf example (with
Asai 2009 and Danvy & Filinski 1990 cited, both DOIs checked against
Crossref before quoting — one previously misremembered DOI elsewhere
in this codebase's history is why that step is never skipped) and the
`Prog` paragraph; TestPrintfAtm (3 tests, including the refutation)
pins every snippet; the doc-snippets ratchet (doc-snippets-pin-all)
passes over the new lines.
