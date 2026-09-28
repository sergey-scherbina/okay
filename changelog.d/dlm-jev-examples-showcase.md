## dlm-jev-examples-showcase — one deterministic test for the Jev example shapes

The separate DLM refund and Jev-quickstart smoke suites are now one offline
`TestDlmJevExamples.showcase`. One run covers the support ticket's billing,
team and urgency decisions; technical and explicitly unowned tickets; the
replayable refund-request action; a scoped confirmation; and the ledger guard
that turns a forged order into `NoPayment` with zero refund calls.

Each assertion names its scenario, but no hosted SDK, network, probability or
Python runtime is involved. The consolidated guide maps the six sections to
their public Jev/SystemOne counterparts and preserves the boundary that only a
payment ledger can establish a financial fact.

Verified with `okayDlm/testOnly okay.dlm.TestDlmJevExamples` (1 passed) and
`affected master Test/compile`. Landed as 0c84f3818 and a67f14de0.
