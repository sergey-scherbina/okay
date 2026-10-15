# The platform on the kernel: what is left after "everything compiles" (2026-10-15)

Companion of [freer-on-core.md](freer-on-core.md). Every module's main, tests and JMH sources compile on the root's words (JVM, JS, Native); the items below were met on the way and wait.

- Walk: Op "is last" (no frames after) → cats' onCancel law.
- Warnings pass (std Reader/State/Writer unused imports, AsyncCont, root test warnings, feature warnings).
- Known changes in freer-on-core.md: Wf.replay/persist no compile-time replay refusal; ZIO error union; Chunks.dropWhile; Push Waiting.
- okay.freer data types still imported: Aggregator, Fold, Monoid, Semigroup, Static, Cps, />> → a home outside okay-freer.
- Member: okay.Foreign.Member (okay-stream) → root?
- Perf: re-measure the A/B lanes after the migration (Bound/Mapped nodes, Async drive on Head).
- Interop consolidation: cats/kyo/zio/fs2 walks → Walk.foldMap where it fits.
- Live-tagged suites to run (integrationTest).
- (done 2026-10-15) An indexed-program word: okay-std's `PState.Indexed` on the kernel; PWizard and PState.Threaded on it.
- widen conversion does not fire for an abstract rest (Writer % W + Users + F vs Users + (Writer % W + F)) nor for Users vs Users + Pure: explicit .at needed (okay-jdbc).
- Writer.of returns Writer % A + Pure: every caller writes .at[Writer % A] (okay-http, okay-ui, okay-mcp) — make it Writer % A in okay-std.
- `X ! E + Pure` should fit `X ! E` without .at (seen with abstract E args): Evidence/Within for a sum with Pure.
