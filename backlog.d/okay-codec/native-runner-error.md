- [ ] native-runner-error, RECURRENCE LEDGER — TWO MECHANISMS, NOT ONE
      (split by native-accept-timeout, 2026-09-25). The entry itself is
      closed in BACKLOG-ARCHIVE.md, and what it settled is ONE of them;
      the ledger below then read every later sighting as that one and
      wrote "nothing changes the settled cause" under readings that
      named a different one. They are told apart by sbt's own line:
      B  EXIT 0, NOBODY KILLED IT (settled 2026-09-09): the binary's
         connection ends and it exits 0 while sbt still has a call in
         flight — `Error: Total N, Failed 0, Errors 1` (shape A in
         gate.sh) or `(<m> / Test / executeTests)` RunTerminatedException
         (shape B). Recognised and re-run by gate.sh since 2026-09-09.
      C  THE ADAPTER KILLED IT: the binary did not connect back within
         `ComRunner`'s hard-coded 40 s accept (test-runner 0.5.12,
         `setSoTimeout(40000)`), the adapter logged `Force close …
         Accept timed out` and `destroyForcibly`'d it (exit 137, "fatal
         signal 9"), and sbt fails `(<m> / Test / loadedTestFrameworks)`
         before any test ran, under load (~80–100 on 14 cores in every
         reading that recorded it). WHY the binary misses 40 s is OPEN:
         native-timeline's first 24 min (2026-09-25, load 66–100, 123
         binaries, 67 alive at once) saw no kill and at most 2 binaries
         runnable per sample — a connected binary waits asleep, so the
         lost 40 s may be on sbt's side of the socket, not the binary's. First seen
         2026-09-18 (okayParseNative), named 2026-09-22 (the accept
         line), and read as "unrecognised" by gate.sh until
         native-accept-timeout taught it: shape C is now re-run alone,
         and a rerun lost the same way is `gate: KILLED` (retried),
         never RED — a RED is what made ci-runner revert a green lane
         (closed by ci-runner-revert-confirm, which also stops the runner bisecting any red with no `==> X`).
      Readings below that are C: 2026-09-18 okayParseNative (twelfth),
      2026-09-22 okayOpticsNative (thirteenth), 2026-09-23
      okayAsyncNative; and 2026-09-25's okayAsyncNative (the reverted
      run), okayChainNative and okayConfNative, which are in the gate
      fixtures rather than here. The rest name no call and are
      consistent with B. `scripts/native-timeline.sh` samples every
      Native test binary once a second beside a gate, which is the
      measurement the C readings kept lacking: how long a killed binary
      lived and what state it spent it in. Record a new reading under
      its mechanism; a reading that names neither is its own finding.
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
      2026-09-18, okayParseNative, ONE module, GREEN on the rerun
      (dataflow-durable-stage, whose diff is one test file and prose —
      no Native source, no parse source). TWELFTH occurrence, and the
      FIRST that is worth more than a tally mark, for two reasons.
      IT NAMES THE CALL IN FLIGHT. sbt's line is
      `(okayParseNative / Test / loadedTestFrameworks)`, so the
      process was lost while sbt was LOADING THE FRAMEWORKS — before
      any test of that module ran. Every earlier reading said only
      "the module reports no tests", which is consistent with a death
      anywhere; this one places it at STARTUP, and the settled cause
      ("the connection ends and the binary exits 0 while sbt still has
      a call in flight") now has a specific call attached to it.
      AND `scripts/gate.sh` DID NOT RECOGNISE IT: the verdict was
      "RED — a failure this script does not recognise". The two shapes
      it knows both carry an `Error: Total N, Failed 0, Errors 1` line
      from the module; this occurrence has NO such line, because the
      module never got far enough to report a total at all. What the
      script would have to match is the RPC exception itself
      (`RPCCore$ClosedException` naming `NativeRunnerRPC$RunTerminated`
      `Exception`) together with the same conditions it already
      demands: zero `==> X` anywhere, and no module reporting a
      non-zero `Failed` or `Errors`. Both held here — 96 module totals,
      every one green, 5 547 results against 5 558 on the cold run,
      and 5 559 on the rerun.
      Load at the start of the failing attempt is in the gate log; the
      box was carrying sibling builds as usual. Nothing here changes
      the settled cause, and the third shape is filed so whoever
      teaches the script has the text.
      2026-09-22, okayOpticsNative, THIRTEENTH occurrence and the
      FIRST on okay-optics — a lane that touched no Native source at
      all (core-modules split fixes: build.sbt dependency wiring,
      three test files moved between okay-async and okay-platform,
      docs). Same third shape as the twelfth: `RPCCore$ClosedException`
      wrapping `NativeRunnerRPC$RunTerminatedException`, but this one
      names its own cause —
      `java.net.SocketTimeoutException: Accept timed out` in
      `ComRunner.awaitConnection`, i.e. the 40s the Native runner
      waits for the forked binary to connect back ran out. Ran plain
      `sbt test`, not `scripts/gate.sh`, so no load sample was
      captured; GREEN on `sbt okayOpticsNative/test` alone immediately
      after, 112/112, in 1s. Nothing here changes the settled cause —
      the runner, not the suite — and it is the first sighting that
      pins WHICH wait times out (the accept, not some downstream RPC
      call), which the twelfth occurrence's account did not have.
      2026-09-22, okayActorNative AND okayOpticsNative in ONE gate,
      GREEN on the rerun of both alone. FOURTEENTH occurrence, second
      PAIR (the first was the sixth, 2026-09-17), on a lane that
      touched no Native source at all (Cont/Effects file reorganising:
      Monadic folded into Cont, TypeableK/Effect/DirectSupport split
      out of Effects.scala, Failing merged into Resource.scala, Provide
      merged into Provide.scala). Nothing here changes the settled
      cause; recorded per the ledger's own point — only the fitting
      readings would make it worth nothing.
      2026-09-23, okayCacheNative, GREEN on the rerun alone (10/10):
      `RunTerminatedException` in the affected re-gate of okay-chain
      after a rebase over docs-adapters-merge — a lane that added a new
      Native module (okayChainNative, green) and touched no okay-cache
      source. Recorded per the ledger; nothing changes the settled cause.
      2026-09-23, okayCrdtNative, GREEN on the rerun alone: lost its test
      process in the full-matrix gate of ui-gtk-integration (6495 tests,
      0 failed) — a lane that touched only okay-ui-gtk's test settings and
      the integrationTest alias, no okay-crdt source. Recorded per the
      ledger; nothing changes the settled cause.
      2026-09-23, okayAsyncNative, x402-exact-local's full matrix: the
      native test process exited 137 ("Test runner interrupted by fatal
      signal 9") and ComRunner's accept timed out — on a module whose
      Native side has NO tests (okay-async's src/test/scala is JVM-only);
      load ~80. gate.sh did NOT classify it as a lost process (it printed
      "a failure this script does not recognise"), so this shape — signal 9
      plus accept timeout, no `Failed 0, Errors 1` line — is a gap in the
      classifier worth closing. Green alone the same minute.

      2026-09-23, okaySqlNative, GREEN on the rerun alone: lost its test
      process in the full-matrix gate of scala2-workflow (6871 tests, 0
      failed) — a lane that added okay-scala2-workflow and touched
      okay-scala2-agent, no okay-sql source. Recorded per the ledger;
      nothing changes the settled cause.

      2026-09-23, okayCacheNative, GREEN on the rerun alone: lost its test
      process in the affected-set gate of x402-signers (6970 tests, 0
      failed) — a lane that added okay-x402-signers and touched
      okay-x402-evm/okay-x402-cdp, no okay-cache source. Recorded per the
      ledger; nothing changes the settled cause.
      2026-09-25, okaySqlNative, GREEN on the rerun alone (24 tests): lost
      its test process in the affected-set gate of stack-safety-core (7150
      tests, 0 failed), a core-only lane with no okay-sql source. Recorded
      per the ledger.
      2026-09-25, okayKernelNative, GREEN on the rerun alone (20 tests): lost
      its test process in the affected-set gate of keccak-pure (7512 tests,
      0 failed), a lane with no okay-kernel source. Recorded per the ledger.
      2026-09-25, okayStreamNative, GREEN on the rerun alone (11 tests, 1
      ignored): lost its test process (Errors 4, Failed 0) in the affected-
      set gate of lexical-tail-guard-abort (7366 tests, 0 failed), a lane
      touching Delim/Lexical only. Recorded per the ledger.
