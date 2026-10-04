## handler-single-pass-stage0: the prize re-measured (architecture, not speed) and dispatch by table measured

- FusionBenchmark today, one pass against nested `handle`s: 1.19x on State + Writer built by `foldLeft`,
  1.17x on Throws + State + Writer, and no win (1.01x) on the right-nested shape of a recursion. Bytes are
  −15.5 KB in all three. On 2026-09-27 these were 1.36x / 1.31x / 1.05x; the step engines made the nested
  handlers faster since.
- `DispatchBenchmark` (new prototype), one fused loop over 2/4/8 handlers: a class table against a chain of
  `TypeableK` tests is 0.99x, 0.78x and 0.66x. The table stays flat, the chain grows. specs/handler-single-pass.md,
  Results (history.d handler-single-pass-stage0).
