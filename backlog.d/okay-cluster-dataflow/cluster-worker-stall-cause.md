- [ ] cluster-worker-stall-cause — the four `WorkerMain` processes of
      TestDistributed's "FOUR REAL PROCESSES" stalled at 0% CPU three
      times in default gates on 2026-09-25, always under a loaded box,
      and never alone (3/3 green in isolation). cluster-forked-stall
      moved the test to Live and gave its waits deadlines that FAIL
      with every worker's thread dump (`Workers.ports`/`within`). When
      that failure appears in `integrationTest`, read the dumps. A
      worker blocked in `Class.forName(registrar)` would be a
      class-initialisation deadlock (memory:
      object-val-summons-nested-schema-deadlock). One blocked in
      `Served.serve`'s accept or read would be the protocol. Until
      then this is filed, not guessed. (2026-09-25)
