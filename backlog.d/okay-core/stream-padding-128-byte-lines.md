- [ ] stream-padding-128-byte-lines — this box's cache line is 128
      bytes (`sysctl hw.cachelinesize`, Apple silicon), and the other
      padded counters in okay-stream assume 64: `Growing.Counter`
      (Growing.scala, seven longs each side = 56 bytes) clears a 64-byte
      line and NOT a 128-byte one. Also AdaptiveFifo's `Cells` is one
      bare `AtomicInteger` per part, allocated back to back, so
      neighbouring parts' claims share a line. ring-head-tail-padding
      (2026-09-27) measured what this costs on the Ring: unpadded
      head/tail read 3.2x slower at one producer and 1.3-1.4x slower at
      4 and 16 (specs/channel-known-producers.md, Results). HOW: the
      same A/B, one lane per `Jmh/run`: `Growing` at p=2/4
      (`default_elem`), `adaptive_chunk` p=16 for `Cells`. Widen to
      fifteen longs (Ring.Padded's layout, probed with
      `Unsafe.objectFieldOffset`) or reuse `Ring.Padded`. UNMEASURED.
      (2026-09-27, ring-head-tail-padding)
