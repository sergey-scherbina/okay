## stream-join-windowed - the event-time windowed join by key, and the okay2 port of the sort-merge one

- `Source.joinWithin(l, r, within, lateness)(atL, atR)` (specs/stream-join.md,
  stage 2): the join for streams NOT ordered by key. A row matches on
  arrival every row of the other side with its key within `within` of its
  EVENT time, is held for the rows to come, and leaves once the watermark
  has passed `at + within`. The watermark is `Windows`' (greatest event time
  seen minus `lateness`) over the SMALLER of the two sides — Flink's rule for
  a two-input operator — so a side racing ahead cannot make the other's rows
  late and the merge's interleaving changes no pair; a row behind it is
  dropped and COUNTED (`dropped`), `held` is the live state. A side's end
  frees the other side's store; a join that can produce nothing more
  (`exhausted`) returns, so a finite side against an endless one ends and
  releases it. Nothing reads a clock.
- `WindowJoin` is the machine (`left`/`right`/`leftEnd`/`rightEnd`, no
  pulling), `WindowJoin.stage` the `Stage` over the sides' merged events
  (`Either[Option[row], Option[row]]`, `None` a side's end), and the `Source`
  form is `through` of that stage over the `either`-merge of the two sides
  with their ends marked — so the merge's release law is the join's for
  free. Symmetric hash join (Wilschut & Apers 1991) with `Windows`' eviction;
  Flink interval join, Kafka Streams KStream-KStream join.
- okay2 in step for STAGE 1: `SortMerge`, `Chunks.joinSorted`/`left`/`full`,
  `Source.joinSorted`/`left`/`full` in okay2's zip shape (no early-stop
  release there, as its zip has none). Stage 2 is not ported: okay2's
  `Source` has no `either` yet.
- Additive: no existing body changed. Tests: `TestWindowJoin` (6, JVM + JS +
  Native), `TestSourceJoinWithin` (3, JVM), `TestDocExamplesWindowJoin`;
  okay2 `TestSortMerge` (5, three platforms), `TestSourceJoin` (3, JVM,
  listed in `jvmSuitesOnly`); docs/guide.md §6. Gate: the suites,
  `TestDocSnippets`, `affected master Test/compile` (303), okay2
  `Test/compile` (64), both recscan inventories unchanged.
- Found on the way: a windowed join's release is counted only under a DRIVE
  — the first cut of the finite-vs-endless test ran `runCollect.runWith`
  bare, saw the right pairs and 0 releases, and was wrong to expect one:
  a plain `runWith` has no fiber handler, and only the collector would
  release the scope (`TestSourceZip`'s early stop forks for the same
  reason).
- Commits: cd91b05ee (windowed join), 5095c29ec (okay2 port).
