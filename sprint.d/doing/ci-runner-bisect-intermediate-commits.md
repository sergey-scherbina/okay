- [ ] ci-runner-bisect-intermediate-commits — the runner bisects every
      commit, not every LANDING, and blamed a commit a later commit of the
      same lane had fixed (found 2026-09-26). Range a3fde7b52..7a3155e4c
      went red on TestCoreAsync's timeout and TestReadyMerge (load); the
      bisect's scoped gate at d3d92efdc — parquet-codec's FIRST commit,
      before its own `1aaf8fa76` added the docs index line — was red on
      TestDocsIndex, so the runner "confirmed" and reverted it; the lane's
      tip (9f180a36f) was green. The revert then CONFLICTED (later lanes
      built on it) and was left mid-revert in the MAIN checkout, blocking
      every sibling's `merge --ff-only` until a human ran
      `git revert --abort`. Needs: (1) bisect only landing tips — a
      commit a `release-claim: …, landed as <sha>` names — `exit 125`
      (skip) for the rest; (2) revert a landing as a WHOLE (its commits
      since the previous tip), not its one commit; (3) a revert that
      conflicts aborts itself and alerts the room, never leaves the main
      checkout mid-revert.
