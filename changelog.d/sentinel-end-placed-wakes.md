## sentinel-end-placed-wakes - a placed end mark wakes a receiver; the parked-Lost of sentinel-single-consumer-lost-end, found and fixed

The sighting of 2026-09-29 (a consumer PARKED on a closed channel with
`hasReady=true receivers=1 metEnds=0`) is a lost wakeup in `placeEnd`.
It published the end mark into the ring and woke nobody. That is safe on
the consumer's own path, but a RESUMED receive runs on the waker's
thread, answers `k` first and places the end second. In that gap the
consumer takes its answer, finds the ring empty, registers and parks, and
close's own wake has already run.

- `SentinelChannel.placeEnd` wakes one receiver when it placed a mark, in
  okay and okay2. On the consumer's own path that is one empty poll per
  placement; a woken receive places again only while the end is pending,
  so the chain is bounded by the parts.
- `TestEndPlacedAfterHandoff` (okay-stream cross, okay2-stream) makes the
  gap deterministic on one thread with nested callbacks: red before the
  fix on both channels and in both repos, with the sighting's exact
  state, green after.
- Not caused by the sender-side recheck of 2026-09-28: the six-channel law
  only offers and never parks a sender.
- clojure-coreasync-load-timeout: `TestCoreAsync`'s `into` test joins its
  go-block sibling in `integrationTest`. Read first for a hang: one `put!`
  is in flight and its callback resumes the sender, so nothing is lost on
  that path; the wait is the OS scheduler's under load.

Gate: `affected origin/master staged`, 5511 tests. Red only in TestDelta
and TestModels (the environment's, red before this lane) and in
okay-codec's TestStackBytes, which is green alone and filed as
`stack-bytes-warm-flake`.
