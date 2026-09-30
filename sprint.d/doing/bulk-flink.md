- [ ] bulk-flink — `flink-core` alone carries no DataStream; an
      instance needs flink-streaming-java. The seam's `Any`-element
      choice is what a `DataStream[AnyRef]` instance would do too.
      (2026-09-10: flink-streaming-java and flink-clients are now on
      okay-flink's TEST classpath for the §20 benchmark, so the
      dependency question is answered — a `Bulk` instance would still
      need them in `compile`.)
