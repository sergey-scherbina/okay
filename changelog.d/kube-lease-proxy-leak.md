## kube-lease-proxy-leak - a default gate no longer leaves a `kubectl proxy` behind

Found while reading a stalled gate's process tree (2026-09-25): 99
orphaned `kubectl proxy` processes on the box, one per gate run, the
oldest 19 hours.

- Cause: `TestKubeLease.munitIgnore` was `proxy.isEmpty`, and `proxy`
  was a lazy val that STARTS `kubectl proxy`. munit evaluates
  `munitIgnore` on every run, Live-excluded or not, and `afterAll` does
  not run for a suite whose tests were all filtered out.
- Fix: the probe reads the file system only (`kubectl` on the PATH and
  a kubeconfig); the proxy starts from the first test's `base`;
  `afterAll` stops it only if a test started it; a proxy that does not
  come up is an `assume` skip.
- Proved: a default `testOnly` of the suite leaves the count unchanged
  (the unfixed tree left one behind — reproduced, then killed by pid);
  the Live run is 7/7 green against the real cluster and the count is
  back after `afterAll`. The other 30 `munitIgnore` overrides were
  surveyed: socket probes or short-lived `--version` processes, no
  second case. specs/cluster-pool.md Results, stage 5.
- The 98 orphans still on the box are the operator's to kill by pid.
