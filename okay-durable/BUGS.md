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

## durable-run-scoped-keys — independent runs alias external attempt keys
<!-- status: fixed
     lane: durable-run-scoped-keys
     area: recovery
     gate: okay-durable/src/test/scala/okay/durable/TestRunKeys.scala
     fixed-in: ee8d4b69b -->

Source baseline: same operation name/sequence/fingerprint in two journals
produces the same key; distinct fingerprint Strings can have the same
32-bit hash. TopicJournal run namespaces its storage, not the external key.
Contract: specs/durable-run-scoped-keys.md. Reuse persisted legacy keys;
never repair namespace isolation by regenerating in-flight attempt keys.

Fixed by stable Journal.runId and bounded Base64url run/position keys.
Stored keys remain authoritative, including legacy incomplete recovery
and replay spans. 47 targeted results and affected staged (2110 results)
passed on the rebased lane, with no compile warnings. Process-crash and
concurrency guarantees remain a separate evidence task.
