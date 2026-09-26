- [ ] batch-coordinator-resume — a batch job survives its coordinator
      (operator, 2026-09-26). Streaming journals the coordinator to
      `Checkpoint` and elects a leader with a fence (stages 8, 10); the batch
      `Cluster.run` cannot resume, so a coordinator's death restarts the
      whole job (specs/dataflow.md). Needs: the batch run's finished
      partitions' partials journalled as they arrive, and a new coordinator
      (the same lease) resuming with only the unfinished ones. Gate: a batch
      job over 4 workers, the coordinator killed after half the partitions,
      a new one finishes with the same answer and recomputes only the rest.
