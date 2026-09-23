## runfree-inlined-small-step - answered: not the step count, effCont24's own construction

runfree-inlined-rotation's side finding — a hand-fused `runFreeInlined`
read 0.904x on `effCont24` (24 steps) while reading 1.02-1.05x worse
on every 10 000-op row lane — left an open question: does a small
step count reverse the verdict, or is it `effCont24`'s own shape? A
single-variable sweep answers it: one PREBUILT left-nested chain over
an identity-handled effect, `@Param` N from 4 to 10 000, the same
`runFree`/`runFreeInlined` pair. WORSE at every N, two quiet rounds
agreeing: 1.157x (N=4) through 1.269x (N=10 000), bytes identical
between arms at every point. There is no step count where the
hand-fused rotation wins on a prebuilt tree.
`effCont24`'s win was therefore its OWN construction: `effSteps` is
an `inline def` unrolled at COMPILE TIME into a call chain inside the
same method as `.runWith`, rebuilt fresh per JMH invocation — not a
`foldLeft`-built, `@Setup`-prebuilt tree crossing a method boundary.
A benchmark-construction artifact, not a production lever; nothing
ships that builds a program this way. Not pursued further, no new
entry opened. Code reverted. Rows `rfss-*`.
