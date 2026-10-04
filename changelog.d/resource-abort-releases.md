## resource-abort-releases — a dropped continuation releases its scopes

An `abort` through a `Resource` scope now releases it: the machine throws
`Shift.Discontinued` into the piece it drops, so every scope there
releases (inner first), and then answers the value. A `try` (`Throws`,
`CanTry`) declines that throw, so it cannot run on in what was dropped,
and a release that fails makes the `abort` fail with it. A `shift` body
that drops `k` says so with `Shift.discontinue(k)` (OCaml 5's
`discontinue`; Leijen's deep finalization, MSR-TR-2018-10). A `k` neither
resumed nor discontinued keeps its scopes open, because a stored `k`
cannot be told from a dropped one. That contract is in
docs/effects/resource.md.

Found on the way: when a catch frame's handler threw a NEW exception and
nothing below took it, the machine threw the ORIGINAL instead. That is
fixed (red first).

Cost: an abort through a `try` alone 1.02x (a run-held `Finalizes` bit);
an abort through a scope ~170 ns more, the price of the release the old
code never made (ShiftBenchmark abort_try / abort_scope, history.d).

Spec: specs/handle-frames.md, "A dropped continuation releases its
scopes". Tests: TestResourceDiscontinue, TestHandleFramesResource.
