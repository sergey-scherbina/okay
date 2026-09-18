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
  WHAT IS ACTUALLY OPEN, from the spec's remaining boxes, roughly by
  value — each has its own entry under `backlog.d/okay-cluster-dataflow/`
  or `backlog.d/dataflow/`, which is where the detail lives:
  - EXACTLY-ONCE FROM LOG TO LOG: `dataflow-durable-stage`,
    `dataflow-fenced-commit`, `dataflow-commit-window`. The spec's
    boxes are kill-the-coordinator-between-the-phases and the same on
    a real `KafkaStore`. This is the one a user would notice.
  - THE OPTIMIZER pushing a combine below an exchange — open since
    stage 2 and marked "STILL NOT" in the spec, which is honest: the
    crossover was measured and the rule declined on the Wrocław job.
  - RESUMING: `dataflow-recovery`, `dataflow-coordinator-election` — a
    batch run that resumes, and a successor that starts by itself.
  - RESCALING: `dataflow-rescale`, `dataflow-rescale-windowed`.
  - THE HARSH TESTS: `dataflow-netem` (injected latency and loss, a
    real partition between coordinator and worker) — the spec says
    these are unmeasured, not passed.
  Nothing here blocks anything else; pick by value, not by order.
