## okay2-join-within - the event-time windowed join in okay2

- `WindowJoin`, `WindowJoin.stage` and `Source.joinWithin` in okay2-stream,
  the Scala 2 twin of stream-join-windowed (specs/stream-join.md, stage 2):
  a row matches on arrival within `within` of event time, held until the
  joint min-of-sides watermark passes its reach, late rows dropped and
  counted, a side's end frees the other store, `exhausted` ends the stage.
  The `Source` form is `Pipe.intoIn` of the stage over okay2's own
  `either`-merge with each side's end marked — the shape `chunked` uses.
- Corrects stream-join-windowed's note that okay2 had no `either`: it has
  (SourceOps). The one Scala 2 trap met: `new WindowJoin(…)` inside a
  null-or-instance `if` infers `K` existentially and the invariant class
  refuses itself; the type argument and the val's type are pinned.
- No release law in okay2 (no cancel scope): the finite-vs-endless test
  checks the endless side's production settles instead.
- Additive. Tests: `TestWindowJoin` (6, JVM + JS + Native),
  `TestSourceJoinWithin` (3, JVM, in `jvmSuitesOnly`). Gate: the suites,
  okay2 `Test/compile` (80), recscan unchanged.
- Commits: d8532b942.
