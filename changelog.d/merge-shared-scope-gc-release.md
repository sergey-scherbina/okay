## merge-shared-scope-gc-release - Merge.Shared's scope held while the merge runs

- `Merge.Shared.elements` entered its cancel scope and never named it
  again. `Source.zip` had the same shape (source-zip-lost-pairs). On a
  plain `runWith` nothing held the scope, so a collection mid-run released
  it through its collector door and closed the merged channel under
  running producers. Red first: `TestMergeScopeReachable` (a thread
  collecting beside 3000 warm rounds) failed on `Shared` at round 634
  with a release mid-run; `Ready` stayed green.
- Fix: `Channel.drainedThen(last)` is `drained` that runs `last` where
  the channel ends; `drained` is `drainedThen(pure(()))`. Shared passes
  its scope's EXIT, so the drain loop names the scope and every
  continuation holds it. That is one operation a run, not a Bind after
  the element program.
- Every scope-opening join checked ("count the doors"): `Source.releasing`
  holds its scope in the trailing exit; `ReadyMerge` keeps it as a field
  of the run whose methods are the loop; `Source.zip` was fixed in
  source-zip-lost-pairs. `TestMergeScopeReachable` pins `Shared` and
  `Ready`; `TestSourceZip` pins the zip.
