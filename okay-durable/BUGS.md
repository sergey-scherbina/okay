# okay-durable defects

## durable-withkey-first-attempt — fresh request omits the recorded key
<!-- status: fixed
     lane: durable-withkey-first-attempt
     area: recovery
     gate: okay-durable/src/test/scala/okay/durable/TestWithKey.scala
     fixed-in: 062f8528b -->

Found during the operator-requested durable-execution review, 2026-10-05.
Fresh WithKey calls pass the unmodified operation to execute; only retries
call Journalled.withKey. Repro: provider succeeds, Journal.complete fails,
then a fresh handler retries the original operation. A provider cannot
deduplicate the first unkeyed request against the keyed retry.
Contract and acceptance: specs/durable-withkey-first-attempt.md.

Reproduced red (two actions) and fixed: first and retry transport the
same recorded key. JVM/JS targeted regressions and affected staged gate
passed (2094 results), with no compile warnings.
