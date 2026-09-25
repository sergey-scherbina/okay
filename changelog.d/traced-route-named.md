## traced-route-named - Traced.route names its span, carries the answer's status, lends its ids

What okay-watch's own per-request span door (`Doorway`) had that
`Traced.route` lacked (span-around-async, answered) — so that it can be
`Traced.route`, and okay-watch drops the Doorway.

- `Traced.route(tracer, name)`: the caller names the root span; a route
  TEMPLATE keeps the label set bounded (default: method and path).
- the answer's status on it: `http.status`, and a 5xx is an error where only
  a throw was.
- `Traced.context`: the answering span's ids on this thread while the answer
  runs, none after — for a process-wide logger.
- `Tracer.to(record)`: spans to a function rather than a topic;
  `annotate`/`fail` speak about the span the caller is in.
- TestTraced +3; docs/modules/okay-obs.md. And docs/snippet-debt.txt loses a
  line pinned since mark-glyph-only (TestDocSnippets' ratchet was red).
- Gate: `affected master` 212 GREEN, no warnings; the doc checks 42 GREEN.

Landed as f7524d491.
