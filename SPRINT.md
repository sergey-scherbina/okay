# Sprint

## Doing
- dataflow — our own distributed engine (specs/dataflow.md). Stage 0
  (the spec) and stage 1 (the plan as a value + the local runtime)
  have LANDED, and so has stage 2 (the exchange, with the crossover
  measured at ~100 000 accumulators — and DECLINED on the Wrocław
  job, correctly), and so has stage 3 (one pass, many sinks). The
  engine's number against the hand-written lane went 5.5x -> **1.14x**
  with `dataflow-complete-panes`: a partition now finishes every pane
  no other partition can touch, so 122 679 accumulators reach the
  coordinator where ~2.9 million did. Next: `dataflow-run-complete-panes`
  (the single-stage road did not get the rule and is now 7.6x behind
  the fan), `dataflow-fan-overhead` (a third of the fan's time is in
  none of its sinks). STAGE 4 IS DONE: the engine runs across four
  real processes, jobs by name, partials by Schema. Next is stage 5,
  failure — a worker that dies takes the run with it today.
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

## Queue
- ui-html-host — the plain road's pure half as an okay-ui host
  (specs/ui-html.md, stage 0 the spec LANDED 2026-09-17). `Live.html`/
  `plain`/`step` are three pure functions of strings that live in
  okay-script, which drags the compiler; okay-watch (a submodule
  consumer with its own router and door) needs them without it. Stage
  1: `okay.ui.Html` in the shared directory, okay-script's names as
  delegates, TestLive's render test and TestLivePlain's step tests
  moved, `okayUiJVM`'s classpath asserted free of okay-script. Stage
  2 (`ui-live-js`, the browser's half of the live road) only when
  okay-watch's specs/ui.md reaches its stage 3.
- applicative-static — the static half of a program
  (specs/applicative-static.md, stage 0 the spec LANDED 2026-09-17).
  `Selective` has had no consumer since it was written; `traverse` over
  `A ! F` can only sequence; nothing lists a program's effects without
  running it. Four stages, each with its deciding test in the spec:
  1 `Par` (parallel applicative over Async, `parTraverse`), 2 `Static`
  (free selective: `leaves` before running, `toFree` to run, `foldMap`
  for batching), 3 the `direct` macro emitting applicative structure
  for independent binds — GATED on 1 and 2 having Results — and 4 the
  theory chapter (S/K/I as Reader, Turner's combinators as the history
  of `Func` and `Fuse`). Stage 1 first: under 200 lines, one
  rendezvous test that cannot pass sequentially.
- THE ENGINE, asked for by the operator 2026-09-17. The architecture
  and the lane order are in specs/durable-workflow.md, stage 4; the
  keystone (`workflow-suspended-driver`) has LANDED, and the rest hang
  off it:
  - workflow-lease — ADVISORY, after visibility: `expect` already makes
    two workers safe, so this only makes collisions rare

### Earlier queue notes

(the note that stood here named eight candidates; SIX have since
 landed — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector, persist-wire, cache-memory, each with a CHANGELOG
 entry — and `mcp-resumable-sse` exists in neither board any more.
 Checked and rewritten 2026-09-09; a queue that names finished work
 sends the next agent looking for it. `ui-durable` survives only as a
 mention inside another BACKLOG entry, not as an item of its own.

 Open and named, for whoever picks next: json-strict-is-now-the-slow-door
 (likely a wontfix — its entry states the disqualifying condition),
 scan-into-the-other-scanners (Yaml/Markdown/Xml/Code still answer the
 pair; wants a measured lane before the conversion).
 scan-step-allocation LANDED 2026-09-10 — the sink road took 29% of
 lexing's allocation, and the arithmetic that predicted a fifth is in
 its CHANGELOG entry.
 lexer-buf-without-concat is GONE from this list: it landed 64d7af6a
 as a refutation (the array candidate measured 11-12% WORSE), which
 also prices what scan-step-allocation was waiting for.
 bracket-over-region is ANSWERED (bracket-pairing, 4651c126): it was
 an unpaired comparison, the crossover is at one operation inside the
 scope, and there is nothing to fix)

- schema-fold — LANDED, all three stages, 2026-09-11 (1d56db5d, aa60c39e,
  e72d44c8; specs/schema-fold.md Results): `Schema.fold`/`Algebra`/`Edge`,
  `Step`/`Folded`, four value doors moved and measured faster, `Validate`.
  What it left in BACKLOG under okay-codec: schema-typed-paths (a checked
  lens on `A`, a macro), form-errors-on-validate (a UI lane). Delete this
  line at the next queue rewrite; it stays one cycle so the next agent
  does not go looking for the stages.

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
