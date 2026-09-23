## windows-stage-rerun-loses-pane - a built pipeline is a value: `through` starts its drive when the program runs

A silent data loss, fixed where the door is. `through(p)(stage)`
resumed the stage at CONSTRUCTION, up to its first await or tell, so
the built program's continuation held whatever mutable state that run
had allocated; running the built program a second time replayed the
first output from the tree and fed the spent state. Measured 2026-09-23
(a java-gatherers probe): `Windows.stage` over events 1,2,15,16,30,
tumbling 10, lost the [10,20) pane on the second run, no error; a JDK
gatherer NPE'd in its finisher, and `Gather.stage` and
`Transducers.stage` had each grown a `finished` flag to refuse the
second run by name.

- `okay-stream/Pipe.scala`: every `through` overload (four) and the
  effectful `pipe` answer `Free.delay(() => loop(...))` — five doors,
  one node per RUN (the `PullBudget` deferral already pays it every
  256 elements). The pure `pipe` answers a plain `B` and is unchanged.
  The rationale is a comment above the first `through`.
- Tests written FAILING first: `TestWindows` runs one built program
  twice over the probe's events and expects three panes both times;
  `TestPipe` counts a stage's starts under `Free.delay` through all
  five doors — 0 after building, 2 after two runs. Finding on the way:
  `!.widen` is a walk and therefore eager, so the counting stage's
  delay sits outside it.
- A SIXTH door, found by that counting test: `!.widen` and
  `Writer.widen` resume the head to walk it, which forces a `Delay` —
  a stage made under `Free.delay` started at WIDEN time and the
  widened value held the start. Both now rebuild `Delay(t)` and
  `Bind(Delay(t), f)` as deferred (`Effects.scala`, `Writer.scala`;
  `TestWidenDelay` in core, failing first: 1 start after widen, now 0
  and one per run). `RowLift.plus`/`at` never walked and never had it.
- `TestGather` and `TestTransducers` flipped: the second run of a built
  program now gives the same windows/batches; what is still refused by
  name is a continuation from INSIDE a run resumed after that run
  finished (multi-shot), reached through `Writer.uncons` over five
  elements so an await remains after the output `rest` is driven to.
  The two guards stay, their messages and comments retargeted.
- Records made false and corrected: `docs/guide.md` (§5 snippet
  comment and paragraph, plus a new paragraph beside the header-parser
  example), `docs/jvm-languages.md`, `docs/modules/okay-java.md`,
  `docs/modules/okay-clojure.md`, `specs/clojure.md` and
  `specs/java-gatherers.md` (their "refuses a second run" boxes and
  results), `Windows.scala`'s and both guards' comments;
  `specs/stage-pipeline.md` gains the semantics line, the box and the
  decision, `specs/event-time-windows.md` the built-program box and the
  result that its old box tested two `through` calls, not one built
  program. Backlog entry removed.
