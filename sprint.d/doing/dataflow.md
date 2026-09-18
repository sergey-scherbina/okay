- dataflow — our own distributed engine (specs/dataflow.md).
  STAGES 0-5 HAVE LANDED, and that includes the two this line used to
  call "next": `dataflow-run-complete-panes` and `dataflow-fan-overhead`
  are both in the changelog, and stage 5 (a worker dies and the job does
  not) has every box ticked. The engine's number against the hand-written
  lane is **1.14x** (from 5.5x) since `dataflow-complete-panes`: a
  partition finishes every pane no other partition can touch, so 122 679
  accumulators reach the coordinator where ~2.9 million did. It runs
  across four real processes, jobs by name, partials by Schema, and a
  worker that throws is buried while its partition is recomputed on a
  survivor.
  WHAT IS ACTUALLY OPEN — checked against the files on 2026-09-18,
  because the list that stood here named FIVE groups and four of them
  were finished: the exactly-once trio (`dataflow-durable-stage`,
  `dataflow-fenced-commit`, `dataflow-commit-window`), `dataflow-recovery`
  and `dataflow-coordinator-election`, `dataflow-rescale`, and
  `dataflow-netem` are all ticked in `backlog.d/`. A sprint entry that
  names finished work is the first thing the next agent reads, so it
  is the worst place for it.
  The five that are genuinely unticked, by value:
  - `dataflow-source-log` (stage 11) — `Flow.topic` over okay-persist
    partitions that SEEK by epoch, and `Sink.stagingTo(topic)` whose
    append IS the commit: exactly-once from log to log on this
    repository's own primitive, verifiable on ONE machine. Its own
    entry says FIRST and that still reads right.
  - `dataflow-rescale-windowed` (stage 13 box 2) — journal a windowed
    operator's OPEN panes so a re-cut can rescale it; today a windowed
    rescale is refused by name, which is the honest half already done.
  - `federation-refusal` (specs/federation.md stage 2) — a worker with
    an allow-list of jobs and a coordinator identity checked before
    the pre-pass.
  - `shipped-terms` — the TASTy-plus-CBOR proposal for shipping a
    lambda to the cluster. A DECISION rather than a task: its entry
    argues the case and names what is actually hard.
  - `dataflow-machines` — BLOCKED and not to be pretended at: it needs
    machines that are not this one.
  AND ONE BOX THAT IS OPEN ON PURPOSE: the optimizer pushing a combine
  below an exchange. The spec says "STILL NOT APPLICABLE" and means
  it — the combine is where a keyed stage BEGINS here, so no plan
  reaches the exchange uncombined and there is nothing to push. It
  stays open only until a stage-4 plan can express one.
  Nothing here blocks anything else; pick by value, not by order.
