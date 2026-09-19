## scoped-cross-platform — okay.Scoped works on Scala.js and Scala Native too
Landed: 2026-09-19

No separate per-platform implementations needed: `ThreadLocal` is
already uniform across JVM/JS/Native in this codebase
(`okay-stream/AdaptiveFifo.scala` proved it first). Moved
`Scoped.scala` from `src/main/scala-jvm/` to the shared
`src/main/scala/`, switched off `ThreadLocal.withInitial` (JVM-only
static factory) to the anonymous-subclass form `AdaptiveFifo.scala`
already uses.

Cross-platform behavior test moved to `src/test/scala-cross/
TestScoped.scala` (JVM+JS); the reflection-based backend check stays
JVM-only (`src/test/scala-jvm/TestScopedBackend.scala`).

The JDK25/ScopedValue Multi-Release variant stays JVM-only by
necessity, unaffected — re-verified after the move.

Verified on real runtimes: okayJVM 5 passed, okayJS 4 passed,
okayNative 4 passed (real native binary), okayScript 208 passed
unchanged.

Gate note: `scripts/gate.sh "affected master"` reported RED from an
unrelated, PRE-EXISTING break in `compare` that this diff's wider
"affected" set exposed for the first time this session — not caused
or worsened by this change (verified by control experiment against
bare master: identical 30 errors either way). See
`compare/BUGS.md:wroclaw-feed-shadowed`. The modules this change
touches are individually green.
