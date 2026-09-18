- measure-proc-walk-load-flake — `MeasureProcWalk` asserts a RATIO and
  a loaded box can make it false. MEASURED 2026-09-18, twice, on the
  same tree: with a sibling agent's gate running beside mine (load
  average 21.7 on 14 cores, their sbt at 397% CPU) it read **1.86x**
  and failed its `large > small * 3` guard, turning a gate RED for a
  lane that had not touched the workflow at all; rerun alone a few
  minutes later on the same commit it read **9.1x**, 186 ns a record.
  The assertion is not wrong — a walk really is linear in the journal,
  and both guards (`> 3x` for "this is measuring the walk" and `< 40x`
  for "the walk is not superlinear") are laws worth keeping. What is
  wrong is that a SINGLE round decides them, and a single round lies
  (`bench-one-round-lies`); the small arm is the one contention
  swamps, so load pushes the ratio DOWN toward the floor guard.
  TWO ROADS, and the choice is a decision rather than an edit:
  (a) per-lane minimum of three rounds, keeping it in the gate — the
      repo's standard answer, and it keeps the superlinearity law
      where every lane pays attention to it;
  (b) tag it `Live` like `BenchCross`, out of the default gate and run
      on purpose — cheaper, and it loses the law from the gate.
  (a) is the better one if three rounds cost little; the suite runs in
  0.127s, so they do. FOUND BY: core-modularise-data's gate, which the
  flake failed; the lane landed after a clean rerun.
