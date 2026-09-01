# okay-obs

Tracing without a framework (specs/obs.md): a span is a VALUE with a
Schema, appended to a persist topic — retention is a Policy,
sampling is which spans you write, shipping is a consumer. W3C
`traceparent` is the one propagation vocabulary; programs stay
observability-blind.

| | |
|---|---|
| `Span` / `Attr` | plain case classes; JSON to look at, CBOR on the topic |
| `Trace.parse` / `render` | the traceparent header, totally — damage starts a fresh root NAMED as such; tracestate rides opaquely |
| `Tracer` | one per request: `root` (the inbound edge) / `span` (a child region) / `outbound` (the header a leaving call carries); `Sample.Never` is a short-circuit by construction |
| `Tracer.traced` | wrap ANY comonadic handler with a span per operation — composition, not instrumentation |
| `Traced.route` | the capability form: a route written against `using Tracer` serves under a per-request tracer; a STORED `Tracer ?=> Route` self-wires at each installation |
| `Otlp` / `OtlpPush` | export is a consumer: spans become OTLP/HTTP JSON for any collector; the offset is the resume token; a refusing collector leaves the batch unconsumed (at-least-once) |

The crossing test follows one traceId from an inbound header through
okay-http into H2 through the Sql seam. No SDK anywhere — a
documented JSON shape is a mapping, not a dependency.
