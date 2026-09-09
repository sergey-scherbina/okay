# Observability: values, topics, and trace context

## Overview

Two-thirds of the observability doctrine already exists and is
practiced: STATS ARE VALUES (plain case classes with Schemas —
persist's `Store.stats`, cache's hit/miss/lag, sql's granted
isolation) and OPS EVENTS ARE TOPICS (persist's meta topic: what
happened at 3am is a `read`, not a grep). The audit found the
missing third: TRACING — nothing in specs/ propagated a trace
context, so a request crossing http → agent → sql → persist could
not be followed across the seams, which is table stakes for
operating a distributed system. This spec completes the doctrine
without importing a framework.

## The model

- **Span = a value.** `Span(traceId, spanId, parentId, name, start,
  end, attrs, status)` — a case class with a Schema, like every
  other observable here. Spans are APPENDED to a trace topic
  (okay-persist), which makes the tracing pipeline the ordinary
  log story: retention is a Policy, sampling is "which spans you
  write", shipping is a consumer, audit is free.
- **Propagation = W3C Trace Context.** `traceparent` (and
  `tracestate`, carried opaquely) is the one wire vocabulary: read
  where requests enter (okay-http server, MCP), written where
  calls leave (http client, sql drivers as comment-free protocol
  attrs, persist-wire frames, REval/PyEval operation attrs). W3C
  because every vendor's collector already speaks it — the
  sslmode logic from specs/tls.md: adopt the operator-legible
  standard, invent nothing.
- **The Durable resonance, stated:** the journal already records
  WHAT the outside world answered (fingerprint, answer); spans
  record WHEN and UNDER WHOM. They share the operation as the unit
  — a journaled operation and its span carry the same identity —
  so an incident replayed by `Durable.replaying` can be laid over
  its original spans. Neither subsumes the other; the spec resists
  merging them.
- **Context carriage**: the current span is HANDLER state, not an
  effect programs request (the Secrets decision again): an
  operation-performing handler opens a child span around the
  operation it performs. Programs stay observability-blind; the
  edge composes a tracing handler around any other, which is what
  handlers are for.
  Between the effect row and hand-threading sits the capability
  form — `Tracer ?=> Route`, specs/context-functions.md
  (obs-traced-routes).
- **Export = a consumer.** SHIPPED (obs-otlp): the exporter reads
  the trace topic and POSTs OTLP/HTTP JSON to any collector's
  /v1/traces — no SDK, because a documented JSON shape is a
  mapping, not a dependency. The offset is the resume token; a
  refusing collector leaves the batch unconsumed, so retry
  re-ships: at-least-once, which is what trace ingestion expects.
  Proven against a recording fake collector (shape, statuses,
  nanos-as-strings, resume, refusal). A JSON dump of a trace
  remains `read` + the Schema, free.

## Behavior

- [x] an inbound traceparent is parsed totally (damage = a fresh
      root trace, named as such); an outbound call carries a valid
      child traceparent; tracestate passes through opaquely
- [x] a request crossing http → sql produces spans sharing one
      traceId with correct parentage, appended to the trace topic
- [x] the tracing handler wraps ANY Handler without that handler's
      knowledge (composition asserted on a mock)
- [x] a journaled operation's span and journal entry carry the
      same operation identity (the overlay join works) — landed
      obs-durable-overlay: a NEUTRAL `OpTrace` seam in okay-agent
      (no okay-obs dependency) that `Durable.tools`/`replaying` take
      optionally; each operation opens a span carrying `durable.key`
      = the journal `Entry.key` (plus op and seq), and a replay
      stamps the same key with `durable.replay=true` — so the replay
      lays over the original. `okay.obs.Tracer` bridges to `OpTrace`
      in one line; proven both with a fake sink (okay-agent) and with
      a real Tracer over a trace topic (okay-obs, TestOverlay)
- [x] sampling-by-policy: a "never" policy writes nothing and
      costs near-nothing (measured bound); a "root only" policy
      writes roots
- [x] spans round-trip through Schema (JSON inspectable, CBOR on
      the topic)

## The third leg: log lines (2026-09-09, obs-log)

This spec's Out of scope said "logging framework integration — log
lines are not this spec's". The microservices audit disagreed, and
the reason is operational rather than aesthetic: a line nobody can
join to a trace is a line nobody reads during an incident, and
fifty printlns across the modules were what stood in for one. The
doctrine's two rules answer it without importing anything.

**No new signature is minted.** A program that logs is a program
that TELLS, and the core has had that effect all along: `Writer %
Log.Line`. `Log.info("placed", "id" -> "o1")` IS `tell(Line(...))`,
a row that logs reads `A ! (Writer % Log.Line + Async)`, and the
whole Writer algebra applies for free — a test can `Writer.run` a
logging program and get its lines as a value with no handler at all.
What is new is the VALUE and the handlers.

**The correlation is the handler's, not the program's.** The
program says a level, a message and fields. The traceId and spanId
are stamped when the line is written, from the ambient `Tracer` —
which is exactly this spec's ruling for spans ("the current span is
HANDLER state, not an effect programs request") applied one leg
over. A domain function stays free of an observability argument;
a line told with no tracer carries no ids and says so by absence
rather than by a guess.

**The handler is comonadic, so a line is written WHEN TOLD.**
`Say(w)` answers `Unit`, so `Handler[Writer % Line]` is expressible
and writes through. A crash after a log call has already logged,
which is the whole point of logging and the one thing an
accumulating Writer would get wrong.

Three sinks, each a mapping and not a dependency, as `Otlp` is for
spans and `Prom` for metrics:

| sink | what it is |
|---|---|
| `Log.console` | one JSON object per line on stdout — what Fluent Bit, Vector, the Docker json-file driver and a Kubernetes node agent all read with no configuration |
| `Log.topic` | the span treatment: lines as records keyed by traceId, so retention is a Policy, shipping is a consumer, and an incident is a READ rather than a grep |
| `Log.collecting` | the test's: the lines in order, as values |

Behavior:
- [x] the program says level, message and fields; the timestamp and
      the ids are the handler's
- [x] a line is written when it is told — a program that throws
      after logging has still logged
- [x] the level filter drops below its minimum and keeps the rest
- [x] `failure` carries a throwable's class and message as fields
- [x] inside `Tracer.root` and a nested `Tracer.span`, each line
      carries THAT span's ids, and the span it names is really in
      the trace topic; a line outside any span carries none
- [x] `Sample.Never` writes no span, so the lines carry no ids —
      and are still written
- [x] `console` is one JSON object per line, ids at the top level,
      a caller's field that shadows a reserved name kept under
      `field.<name>`, a message with quotes and newlines still one
      line and one parseable object
- [x] `topic` records are keyed by traceId and read back as `Line`s
- [x] `Writer.run` over a logging program yields the same lines
      without any handler

Out of scope, still: an SLF4J/JUL bridge (a foreign framework's
lines are that framework's problem; a `Log.to` lambda is the whole
adapter if someone wants one), MDC-style ambient fields beyond the
trace ids, and a sampling policy for lines (the level is the
filter; a topic's Policy is the retention).

## Out of scope

- metrics protocols (Prometheus exposition, OTLP metrics) — stats
  are values; an exporter is a consumer of values, filed with a
  consumer
- logging framework integration — a FOREIGN framework's lines stay
  its own problem (`Log.to` is the whole adapter). Lines themselves
  are no longer out of scope: see "The third leg" above (obs-log)
- baggage (W3C) — carried opaquely if present, interpreted never,
  until a consumer names a need
- an OpenTelemetry SDK dependency — rejected below

## Decisions

- **Spans as data on a topic, not an SDK** — the whole existing
  doctrine extended; the SDK would bring a runtime, a context
  implementation, and an export pipeline this stack already has
  better versions of (handlers, topics, consumers). Rejected:
  opentelemetry-java as a core dependency (stays possible INSIDE
  the OTLP exporter interop, where it is a wire detail).
- **W3C traceparent as the only propagation vocabulary** — every
  collector speaks it; B3 and vendor headers are the exporter's
  translation problem if ever. Rejected: pluggable propagators
  (machinery for a need nobody named).
- **Span context lives in handlers** — programs stay pure of
  observability, the same reasoning that kept secrets and journals
  out of operations; composition over instrumentation. Rejected: a
  Trace effect in program signatures.
- **The journal and the trace stay two things** — one is
  correctness (replayable answers), one is operations (timing,
  causality); merging them would couple retention, sampling and
  trust levels that genuinely differ. Rejected: spans-in-the-
  journal.

## Results

Shipped 2026-09-01 (obs-tracing): okay-obs cross-built JVM/JS/Native
over okay + okay-codec + okay-persist. Span/Attr with derived
Schemas; Trace.parse total (all-zero ids, reserved version ff,
uppercase, wrong arity all refuse); Tracer with root/span/outbound/
outboundState and the traced(Handler) combinator — a polymorphic
namer, since F[?] does not reduce. The crossing test drives a real
okay-http server into H2 through the Sql seam: one traceId from the
inbound header, parentage http -> sql, spans on the topic. Sampling
proven as a write decision with a coarse Never bound (10k spans,
no I/O, generous wall clock). One Tracer per request is the
concurrency story v1 — handler state, like a terminal's cursor.
obs-otlp landed.

obs-durable-overlay landed (2026-09-01): the journal/trace identity
join. The identity that survives a replay already existed — the
Durable `Entry.key` (`keyFor`, "the step's position and what it asked
for, nothing per-process"). A journaled operation now opens a span
carrying that key (`durable.key`, plus `durable.op`/`durable.seq`),
and a `Durable.replaying` run stamps the SAME key with
`durable.replay=true` — so filtering spans by `durable.key` lays a
replay's spans over the originals. The coupling is kept OFF the main
graph: a neutral `OpTrace` seam lives in okay-agent (which does not
depend on okay-obs), `Durable.tools`/`replaying` take an optional
`OpTrace` (default None = no span, no cost), and `okay.obs.Tracer`
adapts to it in one line. The two stay two things — the span carries
the identity, it does not merge the journal into the trace. Proven
twice: a fake sink in okay-agent (the stamping), and a real Tracer
over a trace topic in okay-obs (TestOverlay, which Test-depends on
okay-agent — okay-obs is a leaf, so the arrow makes no cycle).
