## load-flakes - four timing flakes back in the gate, the assertions fixed rather than the tags kept

flaky-to-integration took four timing-dependent tests out of the
default gate the same morning and left each backlog entry open for
the real fix. Done here, each by what its verdict actually depended
on:

- compare `TestSupervisionShapes` — "cancelled, not waited for" was
  judged by ELAPSED time (422 ms at load 48–72). The healthy siblings
  are now `Async.await`s that never finish on their own and count
  their cancellations: `par` cancels its one (== 1), `supervised` its
  nine (== 9), `Par.traverse` at least one. A shape that WAITED hangs
  the test instead of passing slowly. All three doors of the file
  changed, not only the tagged one — the other two had the same clock.
- okay-platform `TestAsyncCross` — ordered a 10 ms timer before a
  50 ms sleep (inverted once on Native). Split into two clock-free
  tests: the sleep answers through `runAsync`; and `runAsync` returns
  an UNFINISHED future for a program waiting on a callback only the
  test fires, then completes when it is fired. Green on JVM, JS and
  Native.
- okay-script `TestStorefront` — asserted no time at all; the 30 s was
  munit's DEFAULT timeout on a test that compiles every page. The suite
  declares its own (5 min); a page that does not compile still fails
  by its error.
- okay-reactive TCK §3.13 — the TCK's own wait for a WeakReference to
  clear is the environment's timeout, 300 ms for every case; the
  collector-judged case gets 3 000 ms ("eventually" is the property,
  300 ms was a budget). 39/39 in the gate, the case at 0.44–0.47 s.

All `Live` tags removed; every suite run through the gate on a box at
load 14–20 (twice for storefront and the TCK), green. Landed as 57ef0b39.
