- runfree-inlined-small-step — DONE 2026-09-23, ANSWERED: NOT the
  step count. A single-variable sweep (`RunFreeStepBenchmark`, a
  PREBUILT left-nested chain over an identity-handled effect, `@Param`
  N = 4/24/100/1000/10000, `runFree` vs `runFreeInlined` — the same
  private hand-fused loop `runfree-inlined-rotation` refuted) found
  the inlined variant WORSE at every single N, two quiet rounds
  agreeing: 1.157x/1.069x/1.074x/1.153x/1.269x (N=4..10000), bytes
  byte-identical between arms and rounds at every N. There is no step
  count, small or large, where the hand-fused rotation wins on a
  prebuilt tree — the parent entry's mechanism (a loop merged into a
  loop-shaped caller costs more than the call it saves) holds
  uniformly, confirmed a THIRD time.
  `effCont24`'s 0.904x (the finding that opened this lane) was
  therefore an artifact of `effCont24`'s OWN construction, not of its
  step count: `effSteps` is an `inline def` that UNROLLS at compile
  time into a chain of calls inside the SAME method as `.runWith`,
  and the tree is rebuilt fresh on every JMH invocation — a
  fundamentally different shape from a `foldLeft`-built, `@Setup`-
  prebuilt tree crossing a method boundary as a heap value. This is a
  benchmark-construction artifact, not a production-relevant lever:
  nothing in the shipping library builds a program by unrolling an
  inline recursive macro at a call site the way `effSteps` does for
  a JMH fixture. NOT pursued further — no new backlog entry opened;
  the question the parent's side-finding raised is closed. Code
  reverted (again). Rows `rfss-*`.
