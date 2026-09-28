- [ ] source-zip — `zip` for the ASYNC stream. Today zip exists only
      on the synchronous carriers: `Chunks.zip(pa, pb)`
      (okay-stream Chunks.scala, realigns chunk boundaries, ends at the
      shorter side, spec'd in specs/chunked-streams.md) and it is a
      companion function only — no `pa.zip(pb)` extension beside
      `elements`/`toLazyList`; `Stream.zip` (core Stream.scala) is
      `toLazyList.zip`, a convenience, not a primitive; `Gen.zip` is the
      one lockstep zip that materialises nothing
      (specs/strymonas-zip-fusion.md). `Source` has `merge`, `concat`,
      `either` and no zip at all — Channel.scala's merge comment names
      zip as the shape merge is NOT ("the concurrency zip and ++ cannot
      express"), so its absence is a gap, not a refusal. THE ASK: (1)
      `Source.zip(that)` in the shape of `Source.merge` — a fiber per
      side, the consumer's demand pulling ONE pair at a time, the stream
      ending when either side ends and the other side's fiber released
      (the early-stop release `Merge.scala` already does for merge), a
      `zipWith` over it; (2) the `Chunks.zip` extension, so
      `chunks.zip(other)` reads like the rest of the chunk API; (3) okay2
      has neither `Chunks.zip` nor the Source one — port both, the
      Scala 2 twin keeps parity (specs/okay2.md). Tests: the
      Chunks laws re-used on Source (misaligned sizes, shorter side,
      infinite under take), plus the release law: a zip stopped early
      leaves no fiber and no open channel behind (the `Merge.scala`
      release tests are the template). Additive lane. Related:
      [[stream-key-join]] — a sort-merge join is a zip that skips.
      (2026-09-28, operator ask)
