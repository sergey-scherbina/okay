- [ ] handlers-vs-plain-loop — the baseline every handler number here
      lacked: the SAME 1 000 State+Writer operations as StagedBenchmark
      (a 10-op block in a 100-iteration loop) written as a plain `while`
      loop with a `var` — once with the persistent `Vector` log the
      handlers thread (the effect machinery isolated), once with a
      mutable buffer (what an imperative programmer writes). Measured
      beside stagedHand, stagedDirect and freeDirectNested, one lane
      per `scripts/jmh-lane.sh` run, min of 3, recorded with
      `scripts/history.sh new`, quoted in docs/benchmarks.md. Asked on
      LinkedIn: "was the goto-speed promise of effect handlers
      realised?" (2026-09-26, operator ask)
