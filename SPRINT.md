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
  NEXT: `dialogue-patch`, then `delim-patterns-in-modules`.
  `delim-forward-not-throw` LANDED 2026-09-17 with a POSITIVE verdict,
  and it did cancel later work: region types are no longer needed for
  the nesting case.
  LANDED 2026-09-17: `delim-diagnostics` (the machine names the capture,
  the prompt, the installed delimiters and the rule) and
  `dialogue-replay-discipline` (the sentence replay rests on is a type;
  the inductive encoding was refuted by a spike, subtyping works).
  STOPPING CRITERION, stated so the arc does not become a workflow
  engine by momentum: the work is done when an ordinary engineer can
  write a waiting process, make a NORMAL mistake in it, and get a
  legible message instead of a puzzle. The first three items below are
  that; everything after is operations around the model, and waits for
  a consumer who needs them.

## Queue
- dialogue-nondeterminism — what is LEFT of stage 1 after
  `dialogue-replay-discipline` landed the constraint: a clock, ids and
  randomness as journalled questions, and `perform`. Decide the shape
  of the question type before writing any of it.
- dialogue-patch — stage 2: needed the moment anyone deploys twice.
- delim-patterns-in-modules — okay-agent's Stepper and okay-llm's Cut
  hand-roll patterns that now exist; cheap, and it is the honest test
  of "this simplifies ordinary code".
- dialogue-continue-as, workflow-operations — stages 3 and 4, on a
  consumer's trigger, not on momentum.

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
