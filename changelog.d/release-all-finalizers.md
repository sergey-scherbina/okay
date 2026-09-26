## release-all-finalizers - a throwing finalizer no longer skips the others or hides the original error

- Before this, `Resource.run`/`Resource.open` released with
  `fin.foreach(_())`. A release that threw stopped every release after
  it, so those resources leaked. During a failure its exception
  replaced the program's, so the cause was lost. `bracketNow`'s
  `try … finally` had the same flaw.
- `Resource.releaseAll` / `releaseAfter` now call every finalizer once.
  The first error wins and the others are `addSuppressed` on it,
  Java's try-with-resources and the JVM's form of ZIO's composed
  `Cause`. `Resource.run`, `Resource.open`, `bracketNow` and the Async
  `Failing` guard all use it.
- TestResource: five tests, four of them watched red first. Spec
  specs/core-gaps.md stage 6.
