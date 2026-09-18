- continuations — making delimited control usable rather than merely
  correct (the operator's question, 2026-09-17). THREE LANES LANDED
  today: `continuations-audit` (the four patterns did not COMPOSE —
  `scope`/`collecting`/`pausing` are the halves that nest, plus
  TestDelimLimits pinning what a capture does to Resource, bracket,
  Throws, try/finally, State and depth), `durable-dialogue` +
  `dialogue-snapshots` (the journal in a topic, warm and cold paths),
  and `dialogue-hardening` (four failure modes found by probing: a
  poisoned journal, a silent mis-mapping after a deploy, a side effect
  performed twice, two writers both accepted — plus the second machine
  as a COMPILE error).
  THE PLAN IS THREE SPECS: specs/durable-workflow.md (stages 0-4,
  stage 0 landed), specs/delim-safety.md (stage 0 landed) and
  specs/delim-diagnostics.md (not started). The order below is by
  value per day, and the reason for it is in each entry.
  THE OPERATOR ASKED FOR THE FULL ENGINE (2026-09-17), which lifts the
  stopping criterion this entry used to carry: stage 4 is now the work,
  its architecture is in specs/durable-workflow.md, and the lane order
  is in the queue below. LANDED so far, all gated: continuations-audit,
  dialogue-hardening, delim-diagnostics, dialogue-replay-discipline,
  delim-forward-not-throw, delim-patterns-in-modules, dialogue-asks,
  wf-durable-journal, wf-direct-door, workflow-suspended-driver,
  workflow-timers, workflow-worker, workflow-visibility,
  workflow-signals, workflow-activity-row, workflow-retries,
  workflow-docs, workflow-cancel, dialogue-continue-as,
  workflow-children, workflow-retire, workflow-lease.
  STAGE 4 IS CLOSED (2026-09-17). Eleven lanes, every one gated: the
  suspended driver, timers, the worker, visibility, signals, the
  activity row, retries, the guide, cancellation, bounded history,
  children, retirement and the advisory lease. The engine runs
  workflows that outlive their process, and docs/durable-workflows.md
  is the page to hand somebody.
  STAGE 3 IS CLOSED TOO (dialogue-resume-cache, MEASURED: five touches
  of a waiting run replay once with the cache and five without).
  THE ARC IS CLOSED FOR EVERY STAGE THAT WAS IN SCOPE
  (delim-diagnostics-position, 2026-09-17) — and the first form of this
  sentence said "every behaviour box in all three specs is ticked",
  which was WRONG and is corrected here. An audit found four unticked
  boxes: two in durable-workflow that were done and never ticked (the
  paragraph beneath them still named a blocker that `Wf.Ask` had
  removed), and two in delim-safety stage 2, which is open ON PURPOSE.
  DELIM-SAFETY STAGE 2 REMAINS OPEN, gated: region types (`runST`'s
  scope tag) for evidence that ESCAPES its `delimited`. The cost is a
  type parameter on every signature carrying evidence, including the
  inline doors whose whole design is that a call site writes as few
  type arguments as possible. Nothing has asked for it; stage 0
  catches the real trap and stage 1 made nested machines usable. The last one turned out
  to rest on a wrong assumption — it said the position would have to
  travel in the journal, and it does not: the READER holds the body,
  so it replays the prefix it accepted and names its own line. No
  field, no version bump, no upcast.
  WHAT THE ARC LEFT BEHIND, for whoever picks it up: the operations
  are younger than the model (no scheduler process, `tick(now)` is a
  call you make), and `Timers.due`/`Signals.next`/`Retire.census`
  scan a topic — honest for thousands of runs, wrong for millions.
  Both are stated in docs/durable-workflows.md rather than discovered.
