## cont-on-frames - Cont runs on the segmented frame machine; `Cont.step` retired (operator decision)

- `Cont` is a program of `Cont0`'s row, run by `Frames`: one root `$`
  per run whose `ret` is the user's `k`, every leaf a `Shift0` to it
  (a1dff8be3, cont-step-on-frames' step 2 re-applied to the optimized
  machine, 8c3d63d65). A CPS-transformed body is a program over a lazy
  `k`; an opaque body gets a strict `k`, a nested run of the machine
  counted and switched as before. `step`, `Reentry`, `Pending` and the
  absorbed leaves are deleted: one machine for `Delim` and `Cont`. The
  answer types stay Cont's (`Rep[A, S, R]`), one claim at the boundary.
- Made affordable in four measured steps: the strict `k`'s room and gauge
  in the run's root, scoped around the nested run, the gauge once per
  run (2ce20409b — a fresh gauge per exhaustion asked the OS for the
  stack, 12% of statePara); `Frames.runUnder`, a run starting with its
  root as the first stack (ba040e07e; `Delim.run`'s boundary too);
  prompts compared by `eq` with one shared evidence (32affd998).
- Against the runner it replaced (history.d …-cont-on-frames-probe.tsv):
  contAnswer **1.09x**, statePara **1.85-1.90x**, fib100 **2.62x** — the
  strict `k` is the work left (backlog cont-strict-k). Native's first
  room is 16 levels (a strict `k` is several frames a level now; 64 cold
  levels overflowed TestContStackNative's 128 KB thread). Docs, specs
  and the backlog say what runs Cont now (ea52f719c).
