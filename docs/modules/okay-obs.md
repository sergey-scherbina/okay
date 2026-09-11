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
| `Log` | log lines as VALUES on the core's own `Writer % Line`: the program says a level, a message and fields, the HANDLER stamps the traceId and spanId from the ambient Tracer; `console` (one JSON object per line), `topic` (keyed by traceId), `collecting` (tests) |
| `Otlp` / `OtlpPush` | export is a consumer: spans become OTLP/HTTP JSON for any collector; the offset is the resume token; a refusing collector leaves the batch unconsumed (at-least-once) |

The crossing test follows one traceId from an inbound header through
okay-http into H2 through the Sql seam. No SDK anywhere — a
documented JSON shape is a mapping, not a dependency.

## Logging, joined to the trace

A program that logs is a program that TELLS, so no new effect is
minted: `Log.info(...)` is `Writer.tell(Line(...))`, and a row that
logs reads `A ! (Writer % Log.Line + Async)`.

```scala
def place(o: Order): Receipt ! (Log.Says + Async) =
  for
    _ <- Log.info("order placed", "id" -> o.id)         // no ids, no clock, no logger
    r <- charge(o)
  yield r

given Handler[Log.Says] = Log.console(min = Log.Level.Info, tracer = Some(tracer))
```

The line the collector receives:

```json
{"level":"info","at":1757413200000,"message":"order placed","traceId":"4bf92f...","spanId":"00f067...","id":"o1"}
```

**The correlation is the handler's.** The traceId and spanId come
from the ambient `Tracer` at write time — the same ruling spans
already follow, so a domain function never carries an observability
argument. Outside a span a line simply has no ids.

**A line is written when it is TOLD.** The handler is comonadic
(`Say` answers `Unit`), so a program that throws right after logging
has still logged. That is also why `Log.topic` is a real sink and
not a buffer.

| sink | for |
|---|---|
| `Log.console` | stdout, one JSON object per line — Fluent Bit, Vector, the Docker json-file driver and a Kubernetes node agent read it untold |
| `Log.topic` | the span treatment: records keyed by traceId, retention a Policy, shipping a consumer, an incident a READ |
| `Log.collecting` | a test: the lines, in order, as values |
| `Log.to(write)` | anything else, including a bridge to a foreign framework |

`Log.failure(msg, e)` puts a throwable's class and message in the
fields. A caller's field that shadows a reserved name (`message`,
`level`, `at`, `logger`, `traceId`, `spanId`) is kept under
`field.<name>` rather than dropped. And because the effect is the
core's, a test can skip the handler entirely: `Writer.run` over a
logging program hands back its lines as a value.
