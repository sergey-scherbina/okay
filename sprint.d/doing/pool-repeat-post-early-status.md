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
