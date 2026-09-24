- [ ] wire-read-deadline — `ForeignWorker` waits forever on a far side
      that stops answering. wire-format-givens' mutant (a Haskell worker
      that confirms `configure` and does not switch) was caught only by
      the gate's stall watchdog after 480 s, not by a failing test.
      okay-r's `RSubprocess` already has an optional deadline
      (`timeoutMillis`); the WireLink family should have one too,
      reported as a condition naming the far side and the request.
