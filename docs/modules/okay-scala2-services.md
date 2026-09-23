# okay-scala2-services

The service libraries for **Scala 2.13**. Their builders and values are
used directly (`ActorRef`, `Supervise`, `new Outbox()`, `Log.Line`,
`new Tracer(topic)`, `new Red(name)`, `new KafkaStore(bootstrap)`). These
objects hold the operations that answer programs:

| | |
|---|---|
| `Actors` | spawn (plain or supervised), child, tell, ask, stop |
| `Outboxes` | the transactional outbox (enqueue, relay, pending) and the inbox (once per id) |
| `Logs`, `Tracing` | log lines as a `Writer`, streamed to a sink; a span around a program |
| `Operations` | health, readiness and metrics routes; RED meters; draining |
| `Kafkas`, `Postgres` | a Kafka producer and consumer; a `Db` over okay-pg |

The walkthrough is section 8q of
[okay from Scala 2.13](../scala2.md#8q-services-actors-outbox-logs-traces-ops-kafka-postgres), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
