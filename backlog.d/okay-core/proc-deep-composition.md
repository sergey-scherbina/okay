- [ ] proc-deep-composition — a `Proc` composed dynamically of N steps
      (`steps.reduce(_ andThen _)`, or a loop of `andThen`) is a term N
      nodes deep, and every walk of it — `Proc.foldMap`, the drawer `go`,
      `nodes`, `Wf`'s journal walker `go`/`loop`, okay2's twins — descends
      once per node (stack-safety stage 5, Decision 7: a written bound,
      not a fix, because the arrow's GADT hides an existential type at
      every `Then` and an explicit stack over it is a redesign of the
      fold). TRIGGER: a consumer composing thousands of steps at run time
      — a per-item pipeline, a generated workflow. FIRST STEP when it
      fires: measure where each walk overflows on the default stack (a
      red-first test with `Proc.Arr(identity)` folded N times), then
      either a `Seq` node holding a Vector of steps with the types
      checked at the seam (chains flatten, the fold stays typed), or the
      typed-frame stack. (2026-09-25)
