- [ ] foreign-one-pool — stage 3 of specs/foreign-one.md: ONE `Pool`.
      Today three: `PyWorkers` (routes by handle, parks dialogues), the
      cluster's `Pool`/`PyPool`/`RPool` (+ `Pool.lease` on
      feature/foreign-streams-holds), and `Holds.pyWorkers` (a SECOND set
      of Python processes per module, admitted by foreign-facade stage
      4b) — plus `SupervisedWorker` reopening and replaying beside them.
      One class: `use` (an exchange), `lease` (a program, a partition, a
      dialogue keeps its worker), routing by ref (`generation << 40 |
      local`, SupervisedWorker's scheme, so a stale ref is refused by
      name), `perWorker(recipe)` (Models' WeakHashMap made the pool's),
      supervision (reopen a dead worker, replay an open program from its
      journal). Gate: `Programs.run`, `statefulIn`, `Model.in`, `hold` all
      through it; the LEASE LEAK found on feature/foreign-streams-holds
      (a step answering a non-transient Left threw out of the iterator
      with the lease held) as a test — a leased worker is released on
      every failure path and when a downstream stops pulling.
