## ci-runner-push-after-flake — the runner pushes after its own flake verdict

Until now a whole-build red whose suites were green when re-run alone
ended the turn with "a flake, not a regression; not pushing", and the
next turn re-tested the range fresh — on a shared box that met a
DIFFERENT suite timing out four turns in a row on 2026-09-28
(TestCoreAsyncChannelLaws, TestFileStoreRace, TestCoreAsyncChannelLaws,
TestFaults), each green alone, while local master ran 75 commits ahead
of origin for five hours. Now the runner's own verdict is what a push
waits for: a red that does not reproduce alone, or a culprit green on
its own tree, is PUSHED that turn (`push_range`, one place for the
push), the sighting is recorded per suite in `.work/ci/flakes/<suite>`
(when, over which range), and a third sighting names the suite a
REPEAT OFFENDER in the log — AGENTS.md's "no flaky tests in the default
gate" says such a suite is `Live`-tagged or fixed. Only a red that
REPRODUCES blocks, and it is bisected, confirmed and reverted as
before. The risk the operator accepted: a real intermittent regression
that shows only under load is pushed as a flake — the record and the
nightly full run are the second gate. Selftest cases 10b and 14 now
expect the push and the record, plus the third sighting; PASS under
`sh` and `bash`. specs/ci-staged.md (the confirmation rule and the
Behavior box).
