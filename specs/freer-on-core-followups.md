# The platform on the kernel: what is left after "everything compiles" (2026-10-15)

Companion of [freer-on-core.md](freer-on-core.md). Every module's main, tests and JMH sources compile on the root's words (JVM, JS, Native); the items below were met on the way and wait.

- Dialogue: persist's local Dialogue.Asks/pause/replay → one generic door in okay-workflow (Wf); Wf.Talk.Say with `where` (source position) → persist's 4 reds.
- Walk: Op "is last" (no frames after) → cats' onCancel law.
- Condition 100k nested frames quadratic (kernel level walk).
- Resource regressions (1) dropped k, (2) multi-shot.
- TestDocSnippets ratchet: ~494 doc lines pinned by removed freer suites.
- Warnings pass (std Reader/State/Writer unused imports, AsyncCont, root test warnings, feature warnings).
- Stack-safety inventory (recscan) rows.
- Known changes in freer-on-core.md: Wf.replay/persist no compile-time replay refusal; ZIO error union; Chunks.dropWhile; Push Waiting.
- okay.freer data types still imported: Aggregator, Fold, Monoid, Semigroup, Static, Cps, />> → a home outside okay-freer.
- Member: okay.Foreign.Member (okay-stream) → root?
- Perf: re-measure the A/B lanes after the migration (Bound/Mapped nodes, Async drive on Head).
- Interop consolidation: cats/kyo/zio/fs2 walks → Walk.foldMap where it fits.
- Live-tagged suites to run (integrationTest).
- One root word for a multi-prompt abort (Shift's escape): okay-llm's Cut (Cutting + aborting) and okay-ui's Scope (Exit + aborting) are two copies of it.
- An indexed-program word: okay-ui's PWizard and okay-std's PState.Threaded still on okay.freer.Freer.
- widen conversion does not fire for an abstract rest (Writer % W + Users + F vs Users + (Writer % W + F)) nor for Users vs Users + Pure: explicit .at needed (okay-jdbc).
- Writer.of returns Writer % A + Pure: every caller writes .at[Writer % A] (okay-http, okay-ui, okay-mcp) — make it Writer % A in okay-std.
- Sort.viaTables / Streamed.viaTables (okay-stream) still translate (adding Tables): make them translateInto like Structured.viaTables; Tables.via with Tables fixed left → Split form.
- `X ! E + Pure` should fit `X ! E` without .at (seen with abstract E args): Evidence/Within for a sum with Pure.
