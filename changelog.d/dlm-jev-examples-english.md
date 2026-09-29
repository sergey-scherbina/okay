## dlm-jev-examples-english — the DLM Jev showcase in English, shaped like the Jev SDK; master compiles again

`TestDlmJevExamples` now follows the Jev SDK's own example
(ticofab/scala-jev-sdk, `examples/Triage.scala`): one `Triage` object with
a typed `Team`, the SDK's message ("Help! My payouts have been failing for
3 days…"), and the answers read back as values. Every fixture is English.
Kept: team triage, an unowned message returning `None`, and the ledger
boundary (`Started` / `NoPayment` / asking for a missing order). Dropped as
noise in an example: record/replay and the pending confirmation, both
covered by `TestDecision`. Spec: `specs/dlm-jev-examples-english.md`.

A full `Test/compile` was red on master since d8b12bd3b (app-host):
`TestProvided` sat in okay-http's shared test tree while calling the
blocking `runWith`, so okayHttpJS failed to compile — moved to
`src/test/scala-jvm`; and okay-desktop's `Window.scala` carried seven
discard warnings, now explicit `val _ =`.

Verified: `Test/compile` over the whole build green with no warnings;
`okayDlm/testOnly okay.dlm.TestDlmJevExamples` and
`okayHttpJVM/testOnly okay.http.TestProvided` pass.
