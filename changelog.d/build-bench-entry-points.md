## build-bench-entry-points - one discoverable path in, for build and for benchmarks

Two root-level scripts, both self-documenting (`--help`), both fronting
`scripts/gate.sh` rather than bare `sbt`: `./build.sh` (test, a cold
compile check, packaging a deployable) and `./bench.sh` (JMH in a
module, the `compare` ecosystem lanes, the Wrocław streaming
benchmark, an A/B, the `src/jmh/history.tsv` log). `WORKFLOW.md` at
the root is the map: what the two scripts do and why nothing else is
a valid way to invoke sbt here, plus pointers to AGENTS.md, the module
docs and the specs.

Building `bench.sh` earned its keep before it shipped: routing a real
JMH run through `scripts/gate.sh` surfaced four latent defects that
`sbt test` cannot reach, because Jmh sources are their own
configuration (a fact this repo already knew and had written down —
`jmh-warnings`, 2026-09-03 — and had drifted from since):

- `okayJVM/Jmh/compile` had two silenced-nowhere warnings in
  `SplitBenchmark.scala` ("conversion from Unit to Int will always
  fail at runtime", from inlining `Writer.foldWith`/`loopWith` at a
  concrete `A=Int` where the library's own unreachable-for-Unit arm
  gets surfaced past the inliner). Neither a bare `@nowarn` nor
  `@nowarn("msg=...")` on the annotated method reached it; a
  `src=`-scoped `-Wconf` entry didn't either, apparently because
  inlined code reports its ORIGIN's source to `-Wconf`'s `src` filter,
  not the expansion site. Only a plain `msg=` `-Wconf` entry worked,
  so the suppression is repo-wide rather than file-scoped — logged in
  build.sbt with the full account of what was tried and refused.
- `AsyncDriveBenchmark.scala`, `ParBenchmark.scala`,
  `DirectParallelBenchmark.scala` and `RowLiftBenchmark.scala` still
  lived in the core `okay` module's `src/jmh`, referencing `Async`,
  `Par` and `Direct` — all moved out in the async/direct/platform
  split. Moved to `okay-platform`'s and `okay-direct`'s own new `Jmh`
  configurations (both newly enabled) rather than added back as a
  reverse dependency from `okay`, which would have been the same
  lazy-val cycle the split itself already ruled out
  (`recursive lazy value okayAsync needs type`).
- `okayDirect`'s new `jmh->compile` dependency on `okayPlatform`,
  first written into the CROSS-PROJECT's shared `.dependsOn(...)`,
  broke ivy resolution for the JS variant (`okay-direct_sjs1_3`) with
  "Cannot add dependency ... to configuration 'jmh' ... because this
  configuration doesn't exist" — js/native never enable JmhPlugin.
  Moved to `.jvmConfigure(_.dependsOn(...))`, JVM-only.
- `compare/src/jmh/scala/okay/ResourceBenchmark.scala` was missing the
  same `AsyncFailing.anyRow` import `okay-platform`'s `TestResource`
  needed after the split (Failing's low-priority any-row instance no
  longer lives in `Failing`'s own companion).

Also: `scripts/gate.sh`'s RED-diagnosis path used `comm -23 <(...)
<(...)` — bash process substitution — while this repo and this
session routinely invoke it as `sh scripts/gate.sh`, which breaks on
that exact syntax with "syntax error near unexpected token `(`"
instead of a verdict. Rewritten with temp files, portable to plain
`sh`; `build.sh`/`bench.sh` also now invoke it (and `ab-defaults.sh`,
also bash-only) explicitly via `bash`, matching `gate-retry.sh`'s own
already-documented convention.

Full account of the single-path-verification policy this all serves —
`scripts/gate.sh` for every sbt invocation, not a bare one for "quick"
checks and a "proper" one for landing — is in AGENTS.md.
