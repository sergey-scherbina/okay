- [ ] delimited-next-per-step — PRIORITY: MEDIUM (2026-10-04). Every step of
      `Run.go` that goes through an effect's `Step` allocates a `Next`: C2
      does not inline `step` into `go` ("callee uses too much stack",
      PrintInlining), so escape analysis cannot remove it. The same root as
      cont-frames-register-pressure; cont-strict-k's carrier experiments are
      the history.
