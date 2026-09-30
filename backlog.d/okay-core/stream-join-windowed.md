- [ ] stream-join-windowed — stage 2 of specs/stream-join.md: the join
      by key of two UNBOUNDED, UNORDERED streams, in EVENT time, on the
      machinery specs/event-time-windows.md landed (okay-stream `Windows`:
      `at: A => Long`, the bounded out-of-orderness watermark, `lateness`,
      dropped rows COUNTED). `Source.joinWithin(l, r, within, lateness)(atL,
      atR)`: each side's rows kept per key for `within` of event time past
      their own `at`, a row matched ON ARRIVAL against what the other side's
      window holds, evicted as the watermark (max seen minus `lateness`,
      monotone) passes it, a row behind the watermark dropped and counted,
      never joined — never the machine's clock, so the test is a list of
      timestamped rows, deterministic, like `Windows`' own. The shape is
      stage 1's: a machine of its own (`step` decides, answers what it
      needs) in `SortMerge`'s mould, driven by `SortMerge.source`'s
      fiber-per-side loop — the loop can move into a helper the two share
      then (the spec's "a third two-sided operator" clause). Literature
      for the docs page: symmetric hash join (Wilschut & Apers 1991) is
      this machine without the eviction; Flink interval join and Kafka
      Streams KStream-KStream windowed join are it with one. okay-flink
      today reaches a join only through `Bulk` for the bounded case.
      ALSO: the okay2 port of stage 1 (`SortMerge` is plain Scala;
      okay2-stream has `Chunks` and `Source`, and source-zip's port is the
      precedent — no early-stop release there). Tests: a row matches what
      the other side's window holds on arrival; a held row is evicted as
      the watermark moves past `at + within`; a late row is counted, not
      silently joined; agreement with `joinSorted` on a bounded input
      whose timestamps all fall in one window.
      (2026-09-30, filed by stream-key-join)
