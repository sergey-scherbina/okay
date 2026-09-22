## iteratees-docs - the textbook names the iteratee it already had

The operator's zippers/iteratees question (2026-09-22) found that
Kiselyov's iteratee exists in the library as `Take` + `Writer` +
`pipe` + `Stage` (Pipe.scala) and `FoldUntil` (Fold.scala), and that
no user-facing page said so: `grep -i iteratee docs/` matched only
the Free/Freer citations. A reader who knows the idea by name could
not find it.

- docs/theory/07-logic-streams.md: a new section "Iteratees: the
  consumer as a program" — what an iteratee is and why Kiselyov
  built it, which okay name is which (iteratee/enumerator/enumeratee
  = `Take`/`Writer`/`Stage`, `pipe` the pairing by delimited
  control), a by-hand vs iteratee PAIR (`firstBlank` over an
  `Iterator` against the same consumer as `!.loop` over
  `Take.await`), the two differences from the Haskell original (a
  row entry instead of a transformer, with the effect-forwarding law
  `TestFoldUntilStreams` pins; `FoldUntil` as the iteratee that is a
  fold with a stop), and `yield` as the dual per Lazy v. Yield.
  Two references added, both DOIs resolved through Crossref before
  being written down — the DOI first remembered for Lazy v. Yield
  named a different paper.
- The same chapter had the "tradition has names worth knowing"
  paragraph twice, verbatim (lines 46–60 and 61–75); the copy is
  gone.
- docs/theory/index.md: chapter 7's blurb and author line.
- docs/guide.md §5: one sentence mapping the names, linking the
  chapter.

Docs only; gate `affected master` green with nothing to test.
