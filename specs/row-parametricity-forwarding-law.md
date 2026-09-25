# The forwarding law: a row-polymorphic handler cannot touch what it does not own

## Overview

Biernacki, Piróg, Polesiuk & Sieczkowski, "Handle with care: relational
interpretation of algebraic effects and handlers" (POPL 2018) prove a
free theorem: a term polymorphic in a row `G` cannot interact with `G`'s
operations. For a handler that means its FORWARDING arm — what it does
with an operation it does not own — is forced: the operation is
performed unchanged, once, in its place, and the answer goes back to
the program's own continuation. specs/scoped-effects-laws.md relies on
that arm ("forwarded transparently") without testing it directly, and
"Abstracting algebraic effects" (same authors, POPL 2019) is the
follow-up for when forwarding must also HIDE an effect
(effect-instances-tunnelling, closed through lexical-instances).

This lane makes the theorem a test the gate runs, with the tool the
coherence law used (`Bisim.check`, specs/handler-equivalence-oracle.md):
two programs are the same tree over sampled answers, or the check names
the first path where they differ.

## The law, as written

For a handler `H` of signature `F` over a row `F + G`, and any `G`:

    Bisim.check(H(p), reference(p)) == Same

where `reference` is the smallest interpreter that can be written from
the theorem alone: it answers `F` in place (a pure step for the
tail-resumptive signatures State, Reader and Writer) and RE-EMITS every
other operation with the rest of the walk as its continuation. The
reference is not a handler anybody would ship; it is the definition of
"forwarded unchanged" as code, and the check compares the residual
`G`-tree of the real handler against it. `G` is a spy row of two
signatures the handler has no business with (`Writer % String` and
`Reader % Boolean`), and Reader's sampled answers give every check more
than one path.

## Behavior

- [x] `State.handle`, `Reader.run`, `Writer.run` and `Writer.collect`
      each forward the spy row unchanged: `Same` against the reference,
      on a program that interleaves the handled signature with both spy
      signatures, and on a 200-step loop (TestRowForwarding).
- [x] A mutant that performs a foreign operation TWICE fails the law,
      and the verdict's path names the duplicated operation.
- [x] A mutant that SWALLOWS a foreign `Say` (answers `()` without
      emitting it) fails the law.
- [x] FOUND: `Writer.collect`, `map`, `expand` and `uncons` take the
      caller's `TypeableK[Writer % W]`, so a `byValue` row is split by
      the test `Distinct` accepted it on (map/expand at the identity
      pinned on a two-Writer row).

## Decisions

- **The mutants are mutants of the forwarding arm, on the reference
  side.** A library handler is polymorphic in `G` and cannot fabricate
  an answer for an operation it does not know, so "swallow" cannot even
  be written against it; it can be written against the concrete spy row.
  Bisim is symmetric, so a reference with a wrong arm against a right
  handler is the same check as a wrong handler against the right
  reference. What the two mutants prove is that the law SEES a
  duplicated and a dropped operation — the two ways a forwarding arm can
  be wrong that the theorem forbids.
- **Not `translate` and not `relay`.** `Effects.translate` turns `F`
  into `G` PROGRAMS, so its residual tree contains what the translation
  emits, and a reference would have to duplicate the translation, which
  is not a law but a re-implementation. `Effects.relay` is
  `State.handle`'s own machinery (State.handle is built on it), so it is
  covered by the State row.

## Results

2026-09-25 (TestRowForwarding 6, okayJVM, with TestRowIdentity,
TestDistinct and TestRowCoherence green beside it):

- State.handle and Reader.run: `Same` on the first run, two paths each
  (Reader's two samples), the 200-step loop at depth 2000.
- THE LAW WAS RED ON MASTER FOR Writer.collect, and the verdict named
  it: `Differ after Ask() -> true: left returned (Vector(1, spy true,
  2),20), right performed Say(spy true)`. `collect[Int]` had taken the
  spy's `Writer % String` Say as its own. Cause: `collect` let `loopWith`
  summon `TypeableK[Writer % W]` inside Writer's companion, where the
  by-class default lives, while the caller's `import Writer.byValue.given`
  had satisfied `Distinct` on the finer test — so the row was accepted
  on one test and split on another. `run`, `fold` and `foldUntil` took
  the caller's instance already; `collect`, `map`, `expand` and `uncons`
  do now (`writerStreamIn`, a given, cannot and is documented). Green
  after, with `map`/`expand` at the identity pinned as laws on a
  two-Writer row.
- The two mutant tests assert `Differ` and pass: the duplicated Say is
  on the verdict's path; the dropped Say differs at its first
  absence. They were not run "unmutated" — the collect defect above is
  the watched red of this lane, and a stronger one.
