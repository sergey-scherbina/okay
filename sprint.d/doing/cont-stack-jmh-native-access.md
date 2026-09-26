- [ ] cont-stack-jmh-native-access — the JMH fork runs without
      `--enable-native-access` and without the versioned jar, so every
      cont-stack lane (statePara, fib100, contAnswer) measures the
      COUNT road; the FFM road a user with native access gets on macOS
      arm64 has never been measured. THE LANE: a way to run the same
      lanes on the FFM road (a jvmArgs variant or a second JMH lane),
      then one alternating pair of statePara and fib100 on each road,
      recorded with `scripts/history.sh new`. (2026-09-26, found by the
      continuations perf review)
