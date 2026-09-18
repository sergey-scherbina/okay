- [x] cluster-testfailure-untagged — DONE 2026-09-16 (operator-followups): the
      suite is `Live`-tagged, out of `sbt test`, in `integrationTest`. As
      found: `okay.cluster.TestFailure` binds a
      real `ServerSocket(0)` ("A CONNECTION THAT BREAKS EVERY TIME") and
      spawns worker JVMs, and it is NOT `Live`-tagged, against the
      nio-port-scope rule that every binding suite tags itself. Seen
      2026-09-15 in split-over-either's first gate: `test timed out after
      30 seconds` reported at 315.018 s — a JVM that stood still for five
      minutes, not a slow test — and the matrix came back with 4314 results
      instead of 4424. Alone on the same tree: 9/9 green. The gate does not
      distinguish "the box paused" from "the test is wrong", so either the
      suite carries the tag or the gate learns the 30 s-vs-300 s signature.
      Not fixed in that lane: its claim did not hold okay-cluster.
