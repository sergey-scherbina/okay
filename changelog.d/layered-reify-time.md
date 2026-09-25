## layered-reify-time - reify as `η $ e` is 10% faster, not only lighter

The time owed by layered-stacked, and nothing more: the two
DelimBenchmark.layered lanes in one run at load 3-5. `η $ e` took
58.34 ± 0.35 µs against 64.58 ± 0.68 µs for stage 0's `push(e.map(η))`,
a ratio of 0.90. Together with the 40 B saved per resumption, this
confirms the switch. Recorded in src/jmh/history.d and in
specs/layered-reflection.md Results.
