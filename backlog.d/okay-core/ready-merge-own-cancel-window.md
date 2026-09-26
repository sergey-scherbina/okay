- [ ] ready-merge-own-cancel-window — on the `own`/`adaptive`
      schedulers (a `DriveTask`: cancel = the drive's `stopped` flag) a
      cancel that lands before `Source.mergeReady` first parks is not
      seen by the sources it already registered: measured 2 of 200
      rounds (specs/ready-merge.md, Decisions; Loom: 0 of 200). The
      drive stops at its next operation without running it and calls
      only the canceller of the Await it last parked on. A fix belongs
      where the gap is — the drive has no way to tell a program "you
      were cancelled between operations" — or the merge parks once,
      synchronously answered, before its first step, IF a synchronous
      answer can install the drive's canceller (today `Drive.op` sets
      `unregister` only on the parked branch). TRIGGER: a mergeReady
      user on `own` whose sources hold something a missed cancel leaks
      (a channel receive, a socket read). (2026-09-26,
      ready-merge-cancel-race)
