- [x] workflow-operations — DONE 2026-09-17, every one of them. stage 4, one spec each when picked:
      durable timers, retry policies on `perform`, signals distinct
      from answers, cancellation, a visibility index, a worker pool
      with leases, child workflows. Named so that nobody mistakes the
      model for an engine.
      LANDED as stage 4 of specs/durable-workflow.md, eleven lanes in
      one day: `Timers`, `Worker.retrying`, `Signals` (a mailbox with
      a cursor per name, because a signal may arrive before the run
      waits for it), `Cancels` (cooperative, and the spec says why a
      throw cannot work), `Statuses`, `Leases` (advisory twice over —
      `expect` is the guard), `Children`. The spec has 0 open boxes.
      Ticked late 2026-09-18: the entry that existed so nobody would
      mistake the model for an engine outlived the engine.
