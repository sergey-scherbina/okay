## sentinel-single-consumer-lost-end - the six-channels law's verdict is its own: Starved is logged, Lost fails with the channel's flags

- Every sighting of "the end is delivered when close races offers on six
  channels at once" (SentinelChannel variants 09-24/25/27, CoreAsyncChannel
  three times in one night 09-28) was a bare munit `TimeoutException`:
  `ChannelLawsSuite` kept munit's 30 s while the law's own wait is 5 s +
  60 s, so the diagnosis the law had taken at 5 s was thrown away every
  time. The suite's timeout is 3 minutes now.
- The diagnosis is per implementation: `ChannelLawsSuite.describe` is a
  hook, `CoreAsyncChannel.debugState` names its two close phases, the
  take in flight and both queues, as `SentinelChannel.debugState` did.
- `LateOrLost` answers `Starved(at)` for a thread still RUNNABLE at the
  last deadline — it has its wakeup and no carrier, the loaded
  whole-build JVM's condition, not the channel's — with both snapshots;
  `Lost` is a parked thread only. The law logs a Starved and fails a
  Lost. TestLateOrLost pins it, red first. specs/okay-diagnose.md.
- REPRODUCTION REFUTED: the tree a sibling saw hang 2/2 (a9aa44111) ran
  the suite 2/2 green with the diagnosis in, no LATE at all — the box,
  not the tree, as the item's 240 000 quiet rounds already said. The
  item goes back to the backlog at MEDIUM with what the next red will
  carry; the "seal never retried after the last pop" hypothesis is
  refuted by reading (`Ring.push` decides fullness by the slot's stamp,
  published in `pop` after the head moves, and every consumer path
  that frees a slot calls `placeEnd`).
- Gate: TestLateOrLost, TestChannelLaws, TestCoreAsyncChannelLaws 128
  green, 0 warnings.
