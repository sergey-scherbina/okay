- [ ] okay-cluster-flow-retype-needs-combinator-library — okay-cluster's
      `Flows.scala`/`Flow.scala` cannot move onto the writer carrier
      without a full retype, and a full retype needs infrastructure
      that does not exist yet. Surveyed 2026-09-19
      (okay-cluster-flow-writer-retype, no code landed).
      `Shape[A]` (`Flows.scala`'s internal compiled-plan type) is
      entirely private, but its `source`/`sourceAt`/`out` all produce
      `Chunks[A]`, tracing to `Flow.Src[A](parts: Vector[Long =>
      Chunks[A]])` — PUBLIC, constructed directly by 21 call sites
      across okay-cluster's own tests, `compare/wroclaw`,
      `okay-persist`, and `okay-demo`. `Flow.map`/`.filter` use
      `Chunks.mapWith`/`filterWith`. A full retype needs
      writer-carrier equivalents of `Chunks.mapWith`/`filterWith`/
      `take`/`drop`/`takeWhile`/`dropWhile`/`rechunkWith` — NONE of
      these exist yet; only the fold terminals (`foldLeftWriter`/
      `foldWriter`, `producer-writer-carrier-foldwriter-eager` +
      `writer-stream-specialized-iterator`) do.
      A SCOPED-DOWN MIDDLE GROUND WAS TRIED AND MEASURED, RULED OUT
      (same lane): bridge the existing `Chunks[A]` source into a
      writer `Feed` via `Source.ofProducer` at just the 7 internal
      `Chunks.foldLeft` call sites in `Flows.scala`, keeping
      `Flow.Src` and all public signatures on `Producer`. Measured,
      JDK 21.0.12, N=10000/64: 7.425us/34,080 B/op against plain
      `Chunks.foldLeft`'s 4.966us/10,064 B/op — 1.5x SLOWER, 3.4x more
      garbage. The bridge tax (stage 0's own elementwise finding:
      75-90% overhead) eats the entire benefit `foldLeftWriter`'s own
      fix bought. Bridging a Producer source into a writer program
      just to fold it is never worth it — do not retry this shortcut.
      THE ONLY VIABLE PATH: the full retype, in order — (1) build the
      missing writer-carrier chunk-transformer family, each measured
      against its `Chunks.X` counterpart the way `foldLeftWriter` was;
      (2) retype `Flow.Src`/`map`/`filter` and `Shape[A]`'s internals;
      (3) update all 21 external call sites; (4) test against
      okay-cluster's existing suite; (5) gate JVM+JS (crossProject).
      A genuine multi-session undertaking — claim it as its own arc,
      not a slice of producer-to-writer-carrier's remaining work.
