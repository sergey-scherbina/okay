- measure-proc-walk-load-flake — `MeasureProcWalk`'s control ratio
  fails INSIDE A FULL GATE and nowhere else, and two theories about why
  are already refuted by measurement.
  THE SYMPTOM: `assert(large > small * 3)` read **1.86x** and then
  **2.75x** in two successive full gates, on a tree that had not
  touched the workflow, turning both RED. Run any other way it reads
  about 9x.
  REFUTED 1 — "one round lies". It already takes the MINIMUM OF SEVEN
  (`best(7)`), so the first backlog entry's proposed fix was a fix it
  already had. Written down because the entry was filed without
  reading the code, which is the mistake `the-record-outlives-the-truth`
  is about.
  REFUTED 2 — "the small arm is too small to see under load". Measured
  at load average 43-45 on 14 cores, three runs: the 400 -> 4 000 pair
  read **9.12x, 9.28x, 9.28x** with the small arm steady at 97-98 µs.
  A probe one decade up, 4 000 -> 40 000, read **6.20x, 5.51x, 5.79x**
  — noisier AND lower, so raising the sizes makes it worse, not
  better. (That sublinearity at 40 000 is its own small question.)
  Running the WHOLE okay-workflow suite rather than the one test
  changed nothing: 8.87x, 8.88x.
  WHAT IS LEFT: the difference is the full-matrix gate itself — 108
  projects, a dozen forked JVMs at -Xmx1g each, beside a sibling
  agent's gate. Nothing smaller has reproduced it in four attempts.
  NEXT: run the gate with the A/B probe still in the test and READ the
  two ratios in the context that actually fails; only then choose
  between making the control robust and tagging the suite `Live` the
  way `BenchCross` is. Do not file a third theory without that number.
