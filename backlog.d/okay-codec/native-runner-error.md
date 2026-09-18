- [ ] native-runner-error, RECURRENCE LEDGER (the entry itself is
      closed in BACKLOG-ARCHIVE.md — the cause is settled: the test
      binary's connection ends and it exits 0 while sbt still has a
      call in flight, so the module reports no tests and sbt reports a
      lost process). Recorded here only so the rate stays visible, as
      `scripts/gate.sh` asks on every occurrence:
      2026-09-10, okayCodecNative, one lost process in a full gate,
      GREEN on the rerun of that module alone (bench-native-lanes).
      2026-09-10 23:12, okayCrdtNative, same shape, GREEN on the rerun
      of that module alone (optics-outside-routes-query). Second
      module to show it, which is consistent with the settled cause
      being the runner rather than any one suite.
      2026-09-15, okayLexNative, same shape, GREEN on the rerun of that
      module alone (handle-decompose, a benchmark-and-prose lane that
      changed no Native source at all). Third module, and the first
      occurrence on a lane that could not have caused it.
      2026-09-16, okayActorNative, same shape, GREEN on the rerun of
      that module alone (producer-drains, which touched no Actor
      source and no Native platform). Fourth module.
      2026-09-17, okayActorNative AND okayConfNative in ONE gate, same
      shape, GREEN on the rerun of both alone (wf-durable-journal,
      which touched okay-persist and okay core only). First time TWO
      modules lost a process in the same run, which fits the settled
      cause — the runner, under a box that was also carrying a
      1-minute load of 12-14 when the gate started.
      2026-09-17, okayCodecNative AND okayObsNative, again two in one
      run, GREEN on the rerun of both (workflow-docs, a documentation
      lane plus one Worker method). SIXTH occurrence, and the second
      PAIR in a single day of heavy gating — which is the first
      evidence that the rate rises with how many gates run per hour
      rather than with what any lane changed. Worth measuring before
      anyone tries to fix it: a gate every ten minutes is the new
      condition, and it arrived with gate-quiet-realistic.
      2026-09-17, okayPersistNative, ONE module, GREEN on the rerun of
      that module alone (dialogue-continue-as). SEVENTH occurrence,
      and two things in it cut against yesterday's hypothesis rather
      than for it: a single module on the heaviest gating day so far
      (the pair, not the rate, may be the coincidence), and the FIRST
      time the module that lost its process is one the lane actually
      changed — okay-persist. With seven occurrences across six
      modules one such coincidence is unremarkable, and it is recorded
      because the ledger is worth nothing if only the fitting
      observations go in it.
      2026-09-17, okayNative, ONE module, GREEN on the rerun (a book
      lane whose diff is markdown only). NINTH occurrence, load
      `{ 13.16 26.00 28.81 }` — a busy box this time, where the eighth
      was quiet. Across nine there is no pattern in the load and none
      in what the lane changed; the only constant is the full matrix,
      which is what the settled cause already says.
      2026-09-17, okayCodecNative, ONE module, GREEN on the rerun of
      that module alone (worker-oracle-attempt). EIGHTH occurrence,
      load `{ 5.95 10.43 11.04 }`. Second single-module sighting in a
      row on a quiet-ish box, which continues to weaken the
      "gates-per-hour" hypothesis filed on the sixth: two of the last
      three were singles, not pairs, and neither box was busy. The
      only thing that has held across all eight is the settled cause
      itself — the runner, not any suite.
      2026-09-18, okayCodecNative, ONE module, GREEN on the rerun of
      that module alone (script-storefront-look, whose diff is a
      markdown fixture, a stylesheet and a browser test — no Scala on
      any Native path). TENTH occurrence, load `{ 4.12 6.86 8.90 }`, a
      quiet box. Nothing new: a module the lane could not have
      touched, on a box under no pressure, which is the tenth reading
      that the full matrix itself is the condition.
      2026-09-18, okayCodecNative, same shape, GREEN on the rerun of
      that module alone (proc-notation-branches, whose diff is a core
      macro, its tests and prose — no Native source and no codec
      source). ELEVENTH occurrence, hours after the tenth and on the
      same module, again on a lane that could not have caused it.
