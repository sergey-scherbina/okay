## flaky-timing-fixes - two recently-filed flakes closed

Two flaky-test findings filed against the default gate this week,
neither yet fixed, closed per the "no flaky tests in the default
gate" policy in AGENTS.md.

`TestClojureCancel` ("the default scheduler: a cancel interrupts")
asserted a WALL-CLOCK bound (`ms < 1000` for a 1500ms sleep cancelled
at 200ms) alongside the real property. py-arrow's gate (2026-09-25,
load ~200) read 1337ms with the mark correctly absent — the cancel
had landed, only late; a loaded box cannot fake the mark, only the
clock. Dropped the `ms < 1000` half of both cancel assertions (the
control test's `ms >= 1400` stays — a slow machine cannot make an
UNCANCELLED sleep finish too early, so that bound only ever fails
honestly); `ms` stays in the failure message for diagnosis.
backlog: clojure-cancel-wall-clock (third sighting).

`TestFederation`'s "TWO REAL PROCESSES, each its own party" spawns
two real JVMs and blocks on their start-up announcing a port over
stdout — a `TimeoutException` at load ~48 on 14 cores (ts-facade's
full gate, 2026-09-23), green 10/10 alone a minute later at load 18.
The suite's other nine tests are fast and in-process; tagging the
whole class `Live` (`TestFailure`'s own precedent, same package, same
`WorkerMain` spawn) would have pulled all nine out of the default gate
for one test's problem. Gave it the mixed-suite `liveTest` helper
(`TestChatDemo`'s own pattern: `test(name.tag(new
munit.Tag("Live")))(body)`) instead, so only the two-process test
moves to `integrationTest` and the other nine keep gating by default.
backlog: federation-two-process-timeout.

12 tests green in the default gate (`okayClojure`'s 3, `okayClusterJVM`'s
`TestFederation` 9 of 10 — the tenth now `Live`).
