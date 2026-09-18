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
