## row-parametricity-forwarding-law - the free theorem as a Bisim law, and the Writer door it caught

"Handle with care" (POPL 2018): a handler polymorphic in the rest of
its row cannot touch an operation it does not own, so its forwarding arm
is forced. specs/row-parametricity-forwarding-law.md.

- TestRowForwarding (6): State.handle, Reader.run, Writer.run and
  Writer.collect against a reference interpreter that answers the
  handled signature in place and re-emits everything else, under a spy
  row of two unrelated signatures; a 200-step loop; two mutants (a
  foreign operation forwarded twice, a foreign Say swallowed) fail with a
  path.
- FOUND on the first run: `Writer.collect` took a `Writer % String`'s
  Say into a `Writer % Int` Vector under `Writer.byValue`. `collect`
  summoned Writer's class-test `TypeableK` inside the companion while the
  caller's `byValue` had satisfied `Distinct` on the finer test.
  `collect`, `map`, `expand` and `uncons` now take the caller's instance
  as `run`/`fold`/`foldUntil` did; `map`/`expand` at the identity are
  pinned as laws on a two-Writer row. The `Stream` given for writer
  programs cannot take one and says so.
- docs/equivalence.md: the forwarding law beside the coherence law, with
  the reference.
