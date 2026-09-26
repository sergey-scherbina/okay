- [ ] cont-stack-8mb-flake — PRIORITY: MEDIUM (a flaky test in the default gate).
      `okay.TestContStack` "an explicit 8 MB thread is granted more than a
      2 MB one (exact road)" failed in a full `affected master` gate at load
      25-40 ("8 MB switched 1 times, 2 MB 1", instances-unify, 2026-09-25),
      and passed twice in isolation right after. The test compares stack
      switch counts between two thread sizes, which the box's load and JIT
      timing can equalise. THE LANE: find what makes the two counts equal
      under load (tiering, the margin, a shared gauge), and assert on
      something load cannot move. Tag it `Live` only if it cannot. Owner:
      cont-stack arc. (2026-09-25)
