## left-nested-build-cost - `!.foldM` / `!.each`, and the shape of a program measured: memory, not time

- `!.foldM(xs)(z)(f)` and `!.each(xs)(f)` build a program RIGHT-nested,
  so nothing is rotated. The program is a value that runs again, and it
  is stack-safe at 1 000 000 elements. TestBuildShape has 6 tests.
- BuildShapeBenchmark, with the build included, compares foldLeft over
  flatMap against foldM/each. Time is 1.01x and 1.02x with one handler
  and 1.08x with two (noisy). Allocation is 17-27% lower. The 22
  foldLeft builders in main code are converted for that memory (operator,
  2026-09-27): core Choice, Prob, Maybe, Chronicle; the streams (Pipe,
  JsonParse, Transducers, Gather, PyStream); Agent, Repair, Retrieve,
  Telegram, Terminal, TransportJs. The effect order is unchanged, and
  `foldM` is delayed so `f` still runs at run time, not at build time.
- Corrected: handler-single-pass had read `-prof stack` as "60-70% of a
  foldLeft program is rotation, the shape costs 2.5x". The sampled frame
  runs the user's continuation, so the reading was wrong.
  specs/handler-fusion.md now says so, and the gap it tried to explain is
  filed as `map-flatmap-pair-cost` (a map + flatMap is two binds per
  operation).
