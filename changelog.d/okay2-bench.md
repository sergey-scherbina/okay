## okay2-bench - the first numbers for okay2 against the Scala 3 core

JMH in the okay2 build (`okay2/src/jmh`, sbt-jmh 0.4.8 as in the root
build, on the core's JVM project) with `HandlerBenchmark` mirroring the
Scala 3 core's lanes of the same name — same program, same size, same
annotations — and a setup that checks every lane's ANSWER before
timing it. Measured per-lane, alternating, on JDK 26 in both arms, min
of three accepted rounds (history.tsv `okay2-*`): `stateEffect` 1.83x
the Scala 3 core in time and 1.64x in bytes (+176 B per get/set pair),
`relayPrebuilt` 1.31x (1.18x B), `handlePrebuilt` 1.07x (1.18x B). So
"no inline" is close to free where the handler does not allocate, and
the State loop's per-operation allocation is the gap: that is
`okay2-handler-allocs`, now with its numbers. docs/okay2.md §8 states
them in place of "a backlog item, not a claim".
