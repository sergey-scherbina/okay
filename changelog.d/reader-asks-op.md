## reader-asks-op - `Reader.read`/`lift` are one operation: 1.84x, −32% memory

- `Reader.Asks(f: R => A)` is new. `read` and `lift` used to be the shared
  `Ask` node plus a map, two binds under the caller's flatMap. They are
  one operation now, answered `f(r)`. `run`, `local`,
  `Bisim.Answers.reader`, the two Reader stagers, okay-kyo's `toKyoEnv`
  and the test handlers learn the case.
- BuildShapeBenchmark.readerLift (the new lane): 29.3 → 15.9 µs, 302 →
  206 KB per 1000 steps. There are 17 uses of read/lift in main.
  specs/effect-row-cost.md D5.
