## core-async-direct-platform-split-fixes - closing the gaps left by the okay-async/okay-direct/okay-platform split

The core module split (`okay` -> `okay-async` + `okay-direct` +
`okay-platform`, plus the `DirectEffect`/`DirectCtx` facade left in
the core so it stays usable without `okay-direct`) compiled clean but
had several loose ends, found by cold-compiling and running the full
matrix rather than trusting the warm target:

- `okay-async`, `okay-direct` and `okay-platform`'s js/native settings
  used `+=` on `Test / unmanagedSourceDirectories` instead of `:=`,
  so the shared `src/test/scala` (JVM-only tests leaning on `CanBlock`,
  `Handler[Async]`, `SchedulerFamily`) leaked into JS/Native builds
  and failed to compile there. Restored the `:=` override the old
  monolithic `okay` project used for the same reason.
- `okayOptics` still only depended on `okay`; its tests use `Direct`,
  `Par` and a `Scheduler`, all of which moved out. Added `okayDirect`
  and `okayPlatform` as test dependencies.
- `okayStm` and `okayStream` were missing `okayPlatform` test
  dependencies their own tests needed (`CanBlock`, `SchedulerFamily`).
- `TestGenerate`/`TestResource`/`TestStream` actually run `Async`
  programs, which needs a `Handler[Async]` and therefore a `CanBlock`
  — exactly what `okay-async` deliberately does not supply. Moved
  them to `okay-platform`, which does (and which already depends on
  `okay-async`, so adding the dependency the other way would have
  been a build-definition cycle, confirmed by trying it:
  `recursive lazy value okayAsync needs type`).
- `AsyncFailing.anyRow` lost the "found automatically" trick it had
  as `Failing`'s own companion member at HEAD (`object Failing extends
  FailingLow`); after the split it lives in a separate `AsyncFailing`
  object, so `TestResource` needed an explicit
  `import okay.AsyncFailing.anyRow`.
- `docs/README.md`'s module table and `docs/modules/` were missing
  pages for all three new modules (`okayDeploy`'s own `TestDocsIndex`
  catches exactly this) — added pages and index rows. Also added a
  `README.md` to each of the three, matching every other module's
  convention (short summary + a `Further` table pointing at
  `docs/modules/*.md` and the relevant `specs/*.md`).

Verified cold (removed target dirs, not just `clean`) after every
round of fixes: `Test/compile` and `sbt test` both run 0 warnings, 0
failures across JVM/JS/Native.
