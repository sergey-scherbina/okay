## cont-stack-jmh-native-access - both cont-stack roads on one JMH lane, and what the flag costs

Every cont-stack benchmark ran the COUNT road, and the reason was not
the missing flag: the JMH forks load okay from the Jmh PACKAGE (a
`-jmh.jar` without `Multi-Release`), so the JDK 22 stack reader was
never the class in play. The forks already carried
`--enable-native-access` (the Jmh host inherits `Test / javaOptions`),
and they also carried the tests' `-Dokay.cont.room=64`.

- build.sbt: the Jmh package carries `jdk22/`'s StackRoom under
  `META-INF/versions/22/` with the manifest attribute, and the Jmh host
  drops the inherited `--enable-native-access`. Every lane keeps the
  count road it was recorded on. The same lane with
  `-jvmArgsAppend --enable-native-access=ALL-UNNAMED` runs the EXACT
  road.
- `ContStackRoad`: every fork of FibBenchmark and HandlerBenchmark
  prints its road and the jar its reader came from. That print is how
  the first attempt, a classpath override, was caught doing nothing.
- Measured (MIN of 3 alternating rounds, JDK 26, macOS arm64):

  | lane | room | exact / count |
  |---|---|---|
  | statePara | 64 (inherited) | 1.53x time, 1.057x bytes |
  | fib100 | 64 | 0.99x, bytes equal |
  | statePara | 873 (the derived default) | 0.99x time, 1.009x bytes |

  At the room a user runs, the flag costs nothing and keeps the program
  on its own thread. The 1.53x is the room of 64 forcing many reads.
- docs/cont-stack.md no longer says the flag makes a deep program
  faster; it says what the flag buys, the caller's own thread.
  build.sbt's comment that the benchmarks run at the derived room is
  corrected.
- Filed: `cont-stack-read-bounds-once` (read the bounds once per
  thread, one `ucontext_t` buffer per thread).

Commits: 652192f8d (spec), 603f8454e (build, benchmarks), d4e1c7b0c (results).
