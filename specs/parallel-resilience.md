# P2 — Parallelism and resilience

## Overview
Concurrency the library already has (fibers, channels, merge) grows
into parallel evaluation and fault tolerance. The organizing insight:
our streams are pure programs and re-observation recomputes — that IS
Spark's lineage, so the CHUNK is the unit of parallelism AND the unit
of failure/recompute (the user's direction: per-chunk fault tolerance,
ideas from Spark).

## Parallelism
- `parMap` over Chunks: a chunk per fiber — the natural grain; order
  preserved by sequence numbers; parallelism bounded by a Scheduler
  budget.
- `parTraverse` for effect programs (a fiber per element/batch).
- Pipeline parallelism: a Stage per fiber, Channels between stages,
  chunk-granular hand-off; backpressure by bounded channels (parking,
  already in place) with chunk-aware capacities.

## Fault tolerance (per chunk, Spark-style)
- Retry at the chunk boundary: a failed chunk computation is re-run
  from its program (lineage recompute — free, because programs are
  values and re-observation repeats work deterministically for pure
  sources).
- Retry POLICIES ARE STREAMS of delays (LazyList/Chunks of durations:
  constant, exponential backoff, jittered — all just generators);
  `retry(policy)(program)` over Async + Throws.
- Checkpointing = memoized prefixes (toLazyList IS the mechanism):
  cut lineage length for long pipelines.
- Fiber supervision: restart-on-failure for feeders (merge/buffer
  fibers), a supervisor as a plain Async program; joinEither already
  reifies failure.
- Effectful (non-pure) sources: recompute is NOT free — the contract
  distinguishes replayable sources (Kafka offsets — rewind) from
  non-replayable (a socket) and only offers chunk-retry for the
  former. This distinction becomes a capability on the source.

## Behavior
- [ ] parMap over N chunks on K fibers: result equals sequential map;
      speedup observed on a blocking workload
- [ ] a killed chunk computation is recomputed and the pipeline
      completes with the correct result
- [ ] exponential-backoff retry sequences its delays as specified
      (policy is a stream — test by taking it)
- [ ] a supervised feeder that dies mid-merge is restarted and the
      merge still yields the full element set (replayable source)
- [ ] a non-replayable source refuses chunk-retry at compile time

## Out of scope
- distributed execution (okay-cluster, P7); speculative execution
