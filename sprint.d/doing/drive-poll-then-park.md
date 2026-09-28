- [ ] drive-poll-then-park — poll-then-park for EVERY consumer, not the
      ring merge alone (operator, 2026-09-28: "а что там насчет полл и
      вейт у шаред мержа?"). Today `Async.Await(register, poll)`'s poll
      is honoured by `ReadyMerge` only; the callback drive
      (`Async.Drive.op`) and the blocking runner (`cb.block(reg)`)
      register at once and ignore it, so `Merge.Shared`'s consumer — a
      `drained` over the one queue, run by the plain drive — takes the
      `Wait` and `Pause` givens in its signature and uses neither, and
      so does every `channel.drained` or `buffer(n)(s).drained` consumed
      without a merge, where a consumer that catches up pays a
      registration and a hand-over per catch-up. THE LANE: (1) `Wait` and
      `Pause` move DOWN from okay-stream to okay-async, where the drive
      lives — okay-async gets `scala-jvm-native` and `scala-js` source
      dirs in build.sbt (the `PlatformPause` seam, as okay-stream has
      since ready-merge-chunk-forward's second landing); (2) the drive
      and the blocking runner, on an `Await` whose poll is not null,
      `wait.until(poll answers)` before registering — the strategy and
      the rungs reach them through the runners' entry points
      (`runAsync`, `runWith`/`CanBlock`, the Loom drive) as `using`
      with the companion defaults, so no call site changes; (3) laws by
      poll and rung count, as `TestReadyMerge`'s, on the drive itself;
      (4) MEASURE `okayChunkedShared` with and without (expected parity:
      its consumer never catches up, ~1 park per op) and a single
      `buffer(1024)(s).drained` consumer (expected the gain: each
      catch-up is a registration today). Spec: specs/ready-merge.md's
      open door in the second stage. (2026-09-28, ready-merge-chunk-forward)
