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
