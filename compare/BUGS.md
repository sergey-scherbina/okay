# Bugs — `compare`

The ledger for defects whose fix lands in `compare`'s own sources.

Newest first. Status lives in the machine-readable header, never in the prose.

## wroclaw-feed-shadowed — `okay.wroclaw.Feed` loses to `okay.Feed[W]` under `import okay.*`
<!-- status: fixed
     lane: compare (okay.wroclaw)
     area: compile
     found-by: gate on feature/scoped-cross-platform pulled `compare` into
       "affected" for the first time this session (it depends on okayJVM,
       which that branch touches); the failure is unrelated to that branch
     gate: compare/compile, compare/test
     fixed-in: wroclaw-feed-shadowed-fix (2026-09-19)
-->

**Root cause, confirmed**: Scala 3 ranks an explicit (even wildcard)
import above a same-package top-level member from another file —
`import okay.*` in `OkayLane.scala` wins over `okay.wroclaw.Feed`
(defined in the sibling `Gtfs.scala`, same package, same compilation
batch). **Fix**: exclude the one name from the wildcard —
`import okay.{Feed as _, *}` — one line, `compare/compile` goes from
30 errors to clean.

`compare/src/main/scala/okay/wroclaw/OkayLane.scala` fails to compile on
plain `origin/master` (confirmed on a clean detached worktree at
`d85dc6a6`, `sbt compare/compile`, no other changes applied):

```
[error] -- [E056] Syntax Error: .../OkayLane.scala:108:16
[error] 108 |  def run(feed: Feed, chunk: Int = 256): Job.Result = {
[error]     |                ^^^^
[error]     |                Missing type parameter for okay.Feed
```

cascading into ~30 errors (every `feed: Feed` parameter, then every field
access off it resolving to `Any`).

`OkayLane.scala` does `import okay.*` (and `import okay.given`).
`okay.wroclaw.Gtfs.scala`, same package, same module, defines
`final case class Feed(events: ..., routes: ..., stops: Int, ...)` — no
type parameter. `src/main/scala/Writer.scala:365` (core) defines
`type Feed[W] = Unit ! Writer % W` — genuinely needs one. The compiler is
resolving `Feed` in `OkayLane.scala` to the CORE type alias, not the
same-package case class, even though `sbt compare/compile` compiles both
files in one batch (`compiling 10 Scala sources and 2 Java sources`, not
a partial/incremental subset) — so this isn't an incremental-compile
staleness artifact, it's a real same-batch resolution question: why does
a wildcard import win over a same-package member here. Not investigated
further — out of scope for the branch that found it.

**Not caused by, and not worsened by, scoped-cross-platform** (verified:
reproduces identically on bare `origin/master`, no diff applied) — noted
here rather than fixed, since the fix belongs to this module and the
finder's actual task was elsewhere.
