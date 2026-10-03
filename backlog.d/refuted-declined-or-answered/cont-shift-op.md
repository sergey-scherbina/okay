- cont-shift-op — REFUTED 2026-10-01 (operator ask, probed): Cont over
  its own operation type `Shift = Strict | Lazy`, the machine a
  trampoline, `c / k0` the handler. The Lazy (CPS-macro) road loses
  the delimiter D-F's `k` carries: a lazy `k`'s nodes share the body's
  machine, the next Shift's head form captures the body's rest, and
  d=2 loops to OOM. The boundary IS the root delimiter (master's
  design). Spec: specs/cont-shift-op.md (on feature/cont-shift-op,
  9d85c5105). Do not retake without a boundary that is not `Cont0`.
  RETAKEN AND LANDED 2026-10-03 as cont-run-prompt, with the boundary
  this entry asked for: a `Cont0.Handling` frame per run (a deep handler,
  so `k` carries its run's frame); the d=2 case is a TestCont test.
