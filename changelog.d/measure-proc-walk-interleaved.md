## measure-proc-walk-interleaved - the arms of a ratio must alternate, or the box forges it

`MeasureProcWalk` turned the gate RED three times in one evening, on
trees that had not touched the workflow at all. Its control compares a
4 000-record walk against a 400-record one and wants about ten; it
read **1.86x**, then **2.75x**, then **2.42x**, each time failing
`assert(large > small * 3)` and blocking a landing.

FOUR THEORIES WENT FIRST, AND THREE WERE REFUTED BY MEASUREMENT.

1. "One round lies." It already took the MINIMUM OF SEVEN. The first
   backlog entry proposed a fix the test had had all along, because it
   was filed without reading the code.
2. "The small arm is too small to see under load." Measured at load
   average 43-45 on 14 cores: the 400 -> 4 000 pair read 9.12x, 9.28x,
   9.28x with the small arm steady at 97-98 µs. A probe one decade up,
   4 000 -> 40 000, read 6.20x, 5.51x, 5.79x - noisier AND lower.
   Raising the sizes measures WORSE.
3. "It is the full suite rather than the one test." 8.87x and 8.88x
   with the whole okay-workflow suite running first.
4. "It is the full-matrix gate." A gate run with an A/B probe inside
   the test went GREEN and printed 9.14x. The probe sits after the two
   measurements it could perturb, so it cannot have helped.

WHAT IT ACTUALLY IS. `best(7)` runs the small arm's seven rounds AS
ONE BLOCK and then the large arm's. A bad window a few hundred
microseconds wide covers all seven rounds of the small arm, inflates
it alone, and the ratio collapses toward the floor guard while the
large arm, measured afterwards, is clean. The minimum cannot escape a
transient that is wider than the thing being measured.

THE FIX IS THE SAME DISCIPLINE AS THE MINIMUM: the arms now ALTERNATE
(`bestPair`), so a transient lands on both or on neither and cancels
out of the ratio instead of forging it. Neither guard is weakened -
both still see a minimum over seven rounds. And the whole control is
re-measured once before it is allowed to fail, because a law about the
walk must not be stated by a box that defeated the measurement; that
costs 0.13s.

A SYSTEMATIC BIAS CAME OUT WITH THE NOISE, which is the part worth
keeping. A linear walk over ten times the journal must read 10.0. The
old instrument read 9.1 in every quiet run - a 9% shortfall, stable
enough to look like a property of the walk. Interleaved it reads 10.1:
the small arm had been running first, and paying for it.

The control that the guards still bite: raised to `* 30`, the test
fails at 11.8x, and the retry fires. Restored after.

Found by: core-modularise-data's gate, three times.
