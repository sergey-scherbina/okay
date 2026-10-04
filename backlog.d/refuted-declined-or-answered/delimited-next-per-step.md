- delimited-next-per-step — refuted 2026-10-04 (history.d
  delimited-next-slot; the probe was discarded, never on master). The `Next` that every
  `Step` answers was replaced by ONE mutable slot per run, overwritten by
  each step: no allocation, and `step` stays out of `go`. Slower: statePara
  1.05x, fib100 1.01x, contAnswer 1.08x, delimGenerator 0.98x. Bytes did
  not change on statePara and fib100, so C2 was ALREADY scalar-replacing
  their `Next`. Where they fell (contAnswer -6%, delimGenerator -9%), the
  writes to a shared heap object each step cost more than the allocation
  did. It also needed an erasure cast per step: a `Next`'s type members
  differ from step to step, which is what types the machine. Not worth
  reopening without a new idea. The carrier history is cont-strict-k's,
  and the inlining road is cont-frames-register-pressure's.
