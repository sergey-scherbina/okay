## symdex-okay — symdex serves okay (2026-10-03)

Commits: 5d72b401c.

- `project/plugins.sbt` adds sbt-symdex 0.5.2 (from the symdex GitHub
  Pages resolver): SemanticDB on for the build. `project/SemanticdbJvmOnly.scala`
  turns it off for Scala.js and Scala Native — symdex indexes a
  cross-built source once, from its JVM copy.
- Its price, measured on a clean compile of okay's core and okay-stream
  (main and test): JVM 30 → 34 s with SemanticDB, JS 12 → 16 s (which the
  JVM-only rule now saves). One pair, under load 18-28: an estimate.
- ci-runner's whole-build compile in the main checkout after each landing
  is what keeps the index there fresh; a worktree's is its own gate's.
- `.mcp.json` serves symdex through `scripts/symdex-mcp.sh`: it finds
  the release (`SYMDEX_HOME`, else `~/.symdex/0.5.0`, downloaded once),
  roots the index at the checkout it sits in, and serves 8 lean tools for
  ~644 schema tokens a turn.
- AGENTS.md ("Skills") says when to reach for it — a known symbol, every
  use exactly, a common or overloaded name — and when not: prose, and
  anything not compiled.
- `.symdex/` (the hook's archive) is ignored.

Gate: `affected master staged` — 9 923 tests, one red:
TestHandleFramesLoops "Writer.map" timed out (45 s, load 25-48); alone on
the same tree it passes in 0.83 s. Filed as
`handle-frames-loops-writer-map-timeout`.
