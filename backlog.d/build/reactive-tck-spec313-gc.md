- [ ] reactive-tck-spec313-gc — okay-reactive's
      `PublisherTckTest.required_spec313_cancelMustMakeThePublisherEventuallyDropAllReferencesToTheSubscriber`
      failed at 1.6 s ("did not drop reference to test subscriber after
      subscription cancellation") in parquet-codec's `affected master`
      gate (2026-09-26; build.sbt changed, so every module ran; the lane
      does not touch okay-reactive). load-flakes (2026-09-23) gave it a
      3000 ms budget — yet it failed well inside it, so the verdict may
      come from the TCK's own GC probe rather than the timeout. Re-run
      alone right after: green (39 cases). Second GC/timing sighting in this
      suite after reactive-tck-spec105-timing.
