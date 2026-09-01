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
- **Export = a consumer.** An OTLP exporter reading the trace
  topic and speaking to any collector (Jaeger, Tempo, vendor) is
  an interop, filed, not core; a JSON dump of a trace is `read` +
  the Schema, today, free.

## Behavior

- [ ] an inbound traceparent is parsed totally (damage = a fresh
      root trace, named as such); an outbound call carries a valid
      child traceparent; tracestate passes through opaquely
- [ ] a request crossing http → sql produces spans sharing one
      traceId with correct parentage, appended to the trace topic
- [ ] the tracing handler wraps ANY Handler without that handler's
      knowledge (composition asserted on a mock)
- [ ] a journaled operation's span and journal entry carry the
      same operation identity (the overlay join works)
- [ ] sampling-by-policy: a "never" policy writes nothing and
      costs near-nothing (measured bound); a "root only" policy
      writes roots
- [ ] spans round-trip through Schema (JSON inspectable, CBOR on
      the topic)

## Out of scope

- metrics protocols (Prometheus exposition, OTLP metrics) — stats
  are values; an exporter is a consumer of values, filed with a
  consumer
- logging framework integration — log lines are not this spec's
  concern; ops events already cover the structured case
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
