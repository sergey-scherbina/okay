- [ ] pool-repeat-post-early-status — TestPool "submit: a repeat POST of
      the same journal reuses the stored record" read status "6" where it
      waits for "10", inside reader-asks-op's whole-build gate on a busy
      box (2026-09-27, okay-gate.MUwtCxlGbc). It passed three times alone
      (twice on that branch, once on master), and the change under test
      does not touch okay-pool. The shape: `waitDone` returns a status that
      is not the finished count, so either "done" is observed before the
      value is final, or the run of the second, ignored POST shares
      state with the first. Reproduce the law in a loop under load
      before reading the code.
      REPRODUCED 2026-09-27 (ProbeRepeatPost, not committed): NOT a test
      flake, a WRONG ANSWER. 8 threads each running the repeat-POST body on
      its OWN id and OWN store, under 20 CPU burners: 3 runs of the first
      few dozen finished `Done` with 5, 6 and 9 for a count of 10. A single
      loop of 300 rounds never did, so it takes CONCURRENT runs in one
      process, which sbt gives a whole-build JVM (suites in parallel). The
      checkpoint history of a bad run shows TWO attempts on the same id
      (e=1, e=2 twice, `Lease.solitary`), and `seen=Vector(0)`: the
      coordinator marked done having recorded no extent seen. Suspects: a
      process-global keyed by something other than the run id (sessions
      in `Folded`, the local worker registry `lead` uses with no peers),
      or Attempts letting a second attempt start once the first stopped
      mid-run. NEXT: okay-test's Stress + Diagnosed on this body, then read
      `job.lead`'s no-peer path for shared state.
