- [ ] span-around-async — okay-obs's `Tracer.root(name)(body)` closes
      the span when `body` RETURNS; a route returns `Response ! Async`,
      a program not yet run, so the span measures building it. okay-watch
      wrote its own door (`api/Doorway`, over okay-obs's `Trace`/`Span`
      vocabulary): the span composed INTO the program the way `ops.Red`
      composes timing, and the ids in a ThreadLocal for the run so a log
      line told under the request carries them (and a line from another
      thread carries none, never another request's). That belongs here:
      a `Tracer.around[A](name)(prog: A ! Async): A ! Async` and a
      context a `Log` handler can read. Then okay-watch's Doorway goes.
      ANSWERED 2026-09-25 (the same agent, from okay-watch): okay-obs
      already has the road — `Traced.route` runs the answer to readiness
      INSIDE the per-request root, so the "span closed when the program is
      built" this item names does not happen there. Porting okay-watch's
      Doorway would make a second road beside it. What Doorway keeps, and
      why it stays in okay-watch: the span is named by the ROUTE TEMPLATE
      (bounded label cardinality; Traced names the raw path) and the ids sit
      in a thread-local a process-wide logger reads. If a second user
      wants either, the place is `Traced.route(name = …)` and a context
      accessor on it — not a new type.

