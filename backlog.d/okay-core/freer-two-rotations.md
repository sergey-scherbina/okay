- [ ] freer-two-rotations — PRIORITY: LOW (2026-10-04). `Freer` has two
      normalizations, `resume` and `resumeRun` (which stops at a
      `Suspended` run). Check whether `resume` still has callers that need
      it apart from `resumeRun`.
