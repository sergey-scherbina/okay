# okay-durable defects

## durable-withkey-first-attempt — fresh request omits the recorded key
<!-- status: open
     lane: durable-withkey-first-attempt
     area: recovery
     gate: okay-durable/src/test/scala/okay/durable/TestWithKey.scala -->

Found during the operator-requested durable-execution review, 2026-10-05.
Fresh WithKey calls pass the unmodified operation to execute; only retries
call Journalled.withKey. Repro: provider succeeds, Journal.complete fails,
then a fresh handler retries the original operation. A provider cannot
deduplicate the first unkeyed request against the keyed retry.
Contract and acceptance: specs/durable-withkey-first-attempt.md.
