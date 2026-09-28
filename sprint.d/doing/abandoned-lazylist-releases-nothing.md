- [ ] abandoned-lazylist-releases-nothing — a merge read through
      `toLazyList` (src/main/scala/Stream.scala:83/271: `LazyList.unfold`,
      each step its own `runWith`) and ABANDONED partway never releases its
      cancel scope: no program ever ends, so neither the drive nor
      `Async.runFiber` sees an end to release at, and the merge's feeder
      fibers stay parked on full channels for good. Left open by
      merge-scopes-everywhere (e4fffb422; specs/ready-merge.md, the
      "…and on the BLOCKING schedulers too" decision) — every merge road
      has it, not one mechanism. A fix needs a close the caller can reach:
      an `AutoCloseable` iterator (`Source.iterator` releasing the open
      scopes on `close()`), or a `Cleaner` on the unfold state as the
      backstop. Law first: take 5 of a merged `toLazyList`, drop it, and
      watch `Source.mergeReleases` stay flat (red), then the close.
      (2026-09-28, found by a sibling's review in the room)
