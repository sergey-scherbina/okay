## okay-test - okay-diagnose and okay-test: failures that explain themselves

- **okay-diagnose** (package `okay.diagnose`) has no dependencies and is
  usable in main code. It holds `Flight` (a bounded, thread-safe flight
  recorder), `Diagnostics` and `Diagnostics.around` (a run's notes and
  on-failure snapshots, added to the throwable), `FailureFormat` (how a
  diagnosis joins a failure; ours keeps class and cause and suppresses),
  `Diagnosable[A]` (a component describes its state), and on the JVM
  `Threads` and `LateOrLost` (starved or parked, told apart).
- **okay-test** (package `okay.testkit`) holds what only tests need.
  munit is an OPTIONAL dependency, named only by `Munit`: `Diagnosed` (a
  FunSuite whose failures carry the test's diagnosis, with munit's diff
  kept), `LiveTests`, and `missing` refused by name. On the JVM it adds
  `Load.burners` and `Stress.repeat`. There are 21 tests on JVM/JS/Native.
- Adopted where 2026-09-27's gates flaked. TestChannelLaws uses
  `LateOrLost` and `SentinelChannel` is `Diagnosable`. TestPool records
  every checkpoint save and dumps it on failure. TestOwnMonitor notes
  each run's thread count.
- AGENTS.md has two rules from the operator: every dependency sits behind
  an abstraction and is optional, and a new suite mixes in
  `Munit.Diagnosed` while debugging tools go into these modules.
  specs/okay-diagnose.md.
