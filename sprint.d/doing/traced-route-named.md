- [ ] traced-route-named — what okay-watch's own per-request span door
      (`Doorway`) has that `Traced.route` lacks, so that it can be
      `Traced.route` (span-around-async, answered): (1) the span NAMED by
      the caller (`name: Request => String`, a route template — bounded
      label cardinality; the raw path is not), (2) a `Tracer` that hands
      its spans to a function (`Tracer.to`), not only to a topic, (3) the
      answer's status on its span — `http.status`, and a 5xx is an error,
      where only a throw was, (4) the trace's ids readable by a
      process-wide logger while the answer runs (`Traced.context`).
