## pool-repeat-post-early-status - concurrent cluster runs no longer share a session (a wrong answer, not a flake)

- TestPool's repeat-POST test read 6 of 10 in a loaded whole-build gate.
  The cause was not the test. Concurrent runs in one JVM, with distinct
  ids and stores, answered each other's counts: 0, 1, 5, 6, 9, 20 for
  10. `Cluster.stream` minted session ids as `System.nanoTime() + i`, and
  `Cluster.local`'s `Sessions` table is process-wide, so two runs that
  started in one tick opened the same sessions.
- `Sessions.mint(n)` hands out disjoint blocks from a counter (clock XOR a
  random salt as the start). A new TestPool test runs 8 threads of
  concurrent runs and asserts every Done is 10. It failed 3 of 3 before
  the fix and passes after. specs/cluster-pool.md has the verification.
