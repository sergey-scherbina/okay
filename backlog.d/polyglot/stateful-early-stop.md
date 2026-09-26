- [ ] stateful-early-stop — a stateful stage (`statefulIn`) whose DOWNSTREAM
      stops pulling before the partition's end never calls `finish` and
      never fails, so the interpreter leased for its state is not given back
      until the JVM ends (foreign-one-pool, 2026-09-26: the failure path is
      closed by `Streamer.abandon`; this one needs a close signal `Chunks`
      does not have). Needs: a way for a `Flow.Local` chunk transformer to
      learn its consumer is done — a finaliser on `Chunks`, or the cluster
      engine calling a stage's close — then `abandon` from it. Gate: a
      counting streamer under a downstream `take`, given back once.
