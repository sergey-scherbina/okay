## ring-head-tail-padding - Ring's head and tail no longer share a cache line

- `Ring`'s `head` and `tail` used to be two bare `AtomicLong`s
  allocated back to back, so they sat on one line. They are now
  `Ring.Padded`, an `AtomicLong` subclass with fifteen longs after the
  value. Offsets were probed on JDK 26: value at 16, a 144-byte object.
  That clears this box's 128-byte line.
- Measured A/B, 10 forks per arm, arms alternating: `oneRing_elem` at
  p=1 went 444 -> 137 us, at p=4 926 -> 722, at p=16 3 309 -> 2 358.
  The fork ranges do not overlap. The single-thread control
  `ring_fillDrain` did not move (8.17 / 8.16). `oneRing_chunk` p=16
  gave no verdict: arm B was bimodal.
- Recorded in specs/channel-known-producers.md (Results),
  docs/queues.md and history.d. The same 64-byte padding assumption in
  Growing.Counter and AdaptiveFifo's `Cells` is filed as backlog
  `stream-padding-128-byte-lines`.
