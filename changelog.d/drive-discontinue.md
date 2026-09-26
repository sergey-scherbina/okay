## drive-discontinue - a callback drive cancelled between two steps releases the scopes it drops

- `Async.Drive` (JS, `Schedulers.own`, `runAsync`), cancelled between two
  non-blocking operations, stopped before the next one and dropped the
  rest of the program, Resource finalizers included. It now DISCONTINUES
  the next operation. It reaches it down the left spine of `Bind`s
  without calling a continuation, so no user code runs, and the Async
  `Failing` guard's operations (`GuardedRun`/`GuardedAwait`, a
  `Discontinue`) release their scopes, innermost first. OCaml 5's
  `discontinue`, done by the runner.
- One `Release` per guarded operation: the release runs once across a
  throw, a Left answer, a cancelled wait and a discontinuation.
- TestAsync: a `bracket` spinning `async` steps on `Schedulers.own`,
  cancelled by `timeout`, is released (watched red). Spec
  specs/core-gaps.md stage 8; backlog `logic-cut-releases` filed.
