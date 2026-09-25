- [ ] reactive-tck-spec105-timing — okay-reactive's
      `PublisherTckTest.required_spec105_mustSignalOnCompleteWhenFiniteStreamTerminates`
      failed "Did not receive expected element within 300 ms" in
      csv-line-edge-space's `affected master` gate (2026-09-25, load average
      ~50 on 14 cores); the lane does not touch okay-reactive (okay-stream
      is below it). The suite alone right after: green, 39 results. The TCK's
      default 300 ms is a wall-clock budget on a loaded box; a
      `TestEnvironment` with a longer default timeout for this suite is the
      cheap answer if it recurs. First sighting.
