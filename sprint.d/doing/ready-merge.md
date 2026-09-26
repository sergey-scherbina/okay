- [ ] ready-merge — merge N sources by READINESS without a fiber per
      source: the merged program keeps a ring of the sources' own
      continuations, steps the head (`resume`), sends a tell out and
      the rest to the back, and treats an `Async.Await` as Pending —
      the continuation parks in the source's slot and the callback
      wakes the merge (one waker cell, a queue of woken continuations,
      not of data). No Await anywhere = deterministic round-robin
      interleave. Parallelism stays a per-source choice
      (`Channel.buffer` a side to give it its own fiber). Measured
      against `Source.merge`/`Channel.merge` on MergeBenchmark's 2x500,
      pure and buffered-async lanes. specs/ready-merge.md.
      (2026-09-26, operator ask)
