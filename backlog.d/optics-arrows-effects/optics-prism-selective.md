- [ ] optics-prism-selective — a second `Star` interpretation over
      `Selective[F]` whose `right` lifts the preview into `F` and
      `branch`es, so `Static` through a prism reports BOTH arms where
      the applicative road reports the one taken. One test with the
      matched control beside it.
      WAITS on static-workflow stage 3 (2026-09-18): for an arrow
      that is a TERM the question is already answered — `Proc.leaves`
      reports both sides of a `Left`, and stage 3 runs a prism's step
      on the matching variant. What is left is the `Star[F]` road
      alone; take it only if a consumer wants the applicative `Static`
      through a sum rather than `Proc`. Close it when stage 3 lands
      and nobody has.
