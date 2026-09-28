- [ ] adaptive-blocking-io — the LAST open condition for `adaptive` as the
      default (specs/schedulers.md, "The default" / "What would reopen
      it"; the Wrocław half was met by adaptive-outside-long-fibers-serial,
      00c140980). Five-way blocking TCP: `adaptive` 58 ops/s against
      Loom's 128-132 (0.45x; batch 17.2 vs 7.7 ms) — 64 fibers in
      blocking calls share `workers + overflow` = 28 threads; and the
      bound itself is a law (TestManagedBlocking): more fibers blocked on
      the library's own doors than `workers + overflow` wait for
      something outside the scheduler. Candidates, each to be priced, not
      assumed: (a) a fiber that blocks on a library door hands ITSELF to a
      virtual thread (managed blocking already knows the moment; the
      catch is resuming on the owned worker after), (b) growth past
      `overflow` for library-door blocking only, with a retire rule, (c)
      keep the bound and say so. BAR: blocking TCP within 10% of Loom and
      no loss on the lanes in scheduler-default-decision's table; then the
      default question is reopened with that table re-run. TRIGGER: the
      operator wanting `adaptive` as the default. (2026-09-28,
      adaptive-outside-long-fibers-serial)
