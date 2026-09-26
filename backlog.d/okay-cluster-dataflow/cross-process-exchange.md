- [ ] cross-process-exchange — the hash exchange between machines
      (operator, 2026-09-26). Across processes a keyed stage finishes by the
      coordinator MERGING partial accumulators; the exchange (stage 2) runs
      only inside one process, and two keyed stages in one flow are refused
      ("two of them need an exchange ... stage 2", Flows.scala). Scoring
      (a map) does not need it; large group-bys and joins over all the data
      — feature engineering — do, and the coordinator merge is their
      bottleneck. Needs: workers exchanging hash buckets directly over the
      existing wire (Arrow chunks), reducers on workers, and the fault model
      of stage 5 (a dead reducer's buckets recomputed). Gate: a two-stage
      keyed flow over 4 processes equal to the single-process answer, a
      reducer killed mid-exchange, the answer unchanged.
