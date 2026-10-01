- [ ] refine-dispatch — operator ask 2026-10-01: hierarchical routing by
      document kind in streams/Spark/Flink/Kafka, written as a Scala
      `match` (specs/refine-dispatch.md). Stage 1 (this lane): `Dispatch` —
      typed lanes, `To` made only by a lane or `unrouted`, sub-tables as
      methods, `Routed.under(prefix)`, a table throw rejects one document,
      split over every `Routable` carrier. Stages 2–4 queued:
      refine-dispatch-kafka, refine-dispatch-flink-stream,
      okay2-refine-dispatch.
