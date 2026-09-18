- [x] parse-depth-test-asserts-wall-clock — DONE the same day
      (parse-depth-tests-out-of-the-gate, 63db4156): the suite is
      `Live` and runs in `sbt integrationTest`. The minimum-of-three
      repair was tried and MEASURED to fail — 19.6x and 11.5x with
      minima on both sides, and the 50 000-level test hit munit's 30 s
      timeout at three runs, because the two sides of the ratio differ
      20x in duration and a contended scheduler perturbs the long one
      far more often. The gate that proved the fix ran green at load
      average 65, the condition that had produced five reds.
