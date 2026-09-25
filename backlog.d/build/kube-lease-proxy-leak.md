- [ ] kube-lease-proxy-leak — PRIORITY: HIGH (every gate on the box
      leaks a process). Found 2026-09-25 19:20 while reading a STALLED
      gate's process tree: 90 orphaned `kubectl proxy --port=NNNNN`
      processes (PPID 1), the oldest 19 hours, one per gate run.
      MECHANISM, read from okay-pool/src/test/scala/okay/pool/
      TestKubeLease.scala: the suite is `Live`-tagged and
      `munitIgnore = TestKubeLease.proxy.isEmpty` — but `proxy` is a
      `lazy val` that STARTS `kubectl proxy`, and munit evaluates
      `munitIgnore` to decide whether to skip, so the default gate
      (which excludes Live) still starts the proxy; `afterAll`, which
      destroys it, does not run for a suite whose tests are all
      filtered out. So the proxy is started and never stopped, on every
      `sbt test`, by every lane. THE FIX: do not start anything in
      `munitIgnore` — decide by a cheap probe (`kubectl config
      current-context` exit code, or an env var) and start the proxy
      lazily inside the first Live test, or in `beforeAll`, which munit
      skips for an ignored suite. Also survey the other Live suites
      that shell out for the same shape (`grep -n 'lazy val' over
      okay-*/src/test` where a `ProcessBuilder` sits in one). DONE
      WHEN: a default `sbt test` leaves no `kubectl proxy` behind
      (`pgrep -fc 'kubectl proxy'` before and after is equal), and the
      Live suite still runs against a real cluster. The 90 orphans on
      the box are the operator's to kill (by PID; they are idle).
      Source: lexical-tail-guard-abort's stalled gate, 2026-09-25.
