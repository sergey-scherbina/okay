## foreign-tests-no-global-state - the flake's cause: a job per test, not a shared reducer

foreign-reduce-wire-heal-flake named it: `TestForeignReduce` and
`TestForeignStage` installed each test's batcher/reducer in a GLOBAL
`@volatile var` that ONE registered job read — and an in-process worker
finds a job by name on its own thread, so a partition could read the
reducer of whichever test set it last (a parallel suite, a lingering
task). Seen once red on a busy box, then green twice alone.

- `StageJob`/`ReduceJob` take their batcher/reducer in the constructor;
  `StageJobs.of`/`ReduceJobs.of` register one per test under a name no
  other test uses. `beforeEach` and the shared variables are gone; the 13
  tests are the same tests with their own jobs.
- Items 3 and 4 of the same plan were already done by siblings while this
  waited: `stateful-early-stop` (`Stateful` over `Flow.Owned`/`Scope`,
  `abandon`) and `foreign-one-runtime` (one body per capability over
  `Language[M]`; `mapIn` and the facade's `Streams` on one frame road).
