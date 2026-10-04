- cont-frames-register-pressure — MOOT as a work item 2026-10-04
  (stack-host-three). The segmented-stack machine whose `loop$1` it read
  (74 spills against 34, KontBenchmark.kontResetOnly) was replaced by
  `Delimited`. KEPT AS A LESSON, since the mechanism outlived the machine:
  C2 keeps a loop's live registers in stack slots across an allocation's
  slow path, and a heap cell standing in for a register is worse (a GC
  write barrier at every edge, 1.47-1.54x). On `Delimited` the same
  family showed up again as JIT-shape effects in `Run.go`
  (state-foreign-shape and run-go-jit-profile, 2026-10-04). Read those
  before chasing a bare install/pop delta.
