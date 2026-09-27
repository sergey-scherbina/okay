- [ ] ring-head-tail-padding — `Ring.scala:97-98` keeps `head` and
      `tail` as two `AtomicLong`s allocated back to back, unpadded: a
      consumer's head move and every producer's tail CAS share one
      cache line, so each pop invalidates the line every pusher is
      spinning on. Vyukov's queue pads them; `Growing.Counter`
      (Growing.scala:206-214) already pads here for the same reason;
      the `Cells` AtomicIntegers of AdaptiveFifo are allocated the same
      way. UNMEASURED — this item is the measurement. HOW: the classic
      padding subclass (seven longs each side; `@Contended` needs
      `-XX:-RestrictContended` for non-JDK classes, so not that), one
      lane per `Jmh/run` through `scripts/jmh-lane.sh`: elementwise
      `ManyProducersBenchmark` at p=4 and p=16 (contended tails), the
      chunked twin as the control (one CAS per batch — expected ~0),
      `ChannelBenchmark` single-producer as the no-contention control,
      5 forks, arms alternating. Read p=2 with `two-producer-fork-
      bimodality` (backlog) in mind: it lands on a slow fork 1 in 4, so
      medians, not means. EXPECTED (hypothesis): 5-15% on contended
      elementwise, nothing on chunked; ZERO everywhere is a real
      answer — revert and record it in the spec's Results, so nobody
      pads on faith later. Spec: the Results of
      specs/channel-known-producers.md (the spec that owns the ring's
      partitioning) — add a section; docs/queues.md's table gets the
      new p=16 number if it moves. (2026-09-27, perf-plan)
