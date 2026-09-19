## ci-affected-tests-only - a test-only edit stops paying for everything downstream

`project/Affected.scala`'s dependent closure was seeded by every
touched project — main OR test — but a dependent only ever sees what
`Compile` built, never a sibling's test sources. Measured before the
fix: one comment line added to a single `okay-lex` test file (one
real dependent, `okay-parse`) pulled 52 projects into the gate.

`mainDirs`/`testDirs` split out of `dirs`; `direct` (what still runs
its own tests, unchanged) stays seeded by both, but `closeOverDependents`
is now seeded by `mainChanged` — `Compile` dirs only. A genuine main
change still sweeps its full closure (checked: same okay-lex file
touched under `src/main` still pulled the same 52 projects).

Verified by hand rather than by a new automated test — `project/`
build machinery has no scripted-test harness here — using the
`affected HEAD compile jvm` trick: commit the fix, then diff only an
uncommitted probe file against that commit, so the probe's own diff
doesn't itself trip `buildChanged`.
