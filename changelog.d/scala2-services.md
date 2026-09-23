## scala2-services - okay-actor, -outbox, -obs, -ops, -kafka, -pg from Scala 2.13

Stage 15.8 of specs/scala2-facade.md, the last item of the stage's queue
(operator: "делай всё что возможно чтобы работало в скале 2").

- Probed first: the builders and values of all six libraries are plain
  and used directly from Scala 2 (`KafkaStore` is an okay-persist `Store`,
  so the persist facade works over Kafka unchanged).
- The new module okay-scala2-services wraps only the operations that
  answer programs: `Actors`, `Outboxes`, `Logs`, `Tracing`, `Operations`,
  `Kafkas`, `Postgres`. okay-scala2-sql's `Db` gained a `private[scala2]`
  accessor so the outbox runs on the facade's own connection.
- `TestServicesFromScala2` (6, offline: actors plain and supervised, the
  outbox relay and the inbox over H2, logs streamed, a span written, ops
  routes and a RED meter). `TestServicesLiveFromScala2` (2, Live, out of
  the default gate): run here against `postgres:16` and
  `apache/kafka:3.9.0` in throwaway containers, GREEN.
- Two of the lane's own mistakes, caught by those tests before landing:
  the ops endpoints are `/healthz`/`/readyz`, and okay-pg's SQL numbers
  its placeholders (`$1`) where JDBC writes `?`.
- Docs: section 8q of docs/scala2.md (copied from the probe), the module
  page, the API reference, and the spec's stage 15.8.
- Gated: the full matrix, GREEN (6898 tests, cold, no warnings), on the
  tree before master gained e435d179/5b94834f (ts-api-client: okay-http's
  Route, okay-codec's Stubs). After rebasing onto them, the re-gate was
  scoped to where the lanes meet, `okayScala2Probe/test; okayHttpJVM/test;
  okayOpsJVM/test` with the probe compiled cold: GREEN, 269 tests.
