- [ ] okay-watch-pointer-bump-check — okay-watch (private, consumes okay
      as a submodule) restores through `Blob.get -> Producer.each` and
      may build `Chunks` with `produce(...)`; producer-to-writer-carrier
      changed both (`Blob.get` is a `Source`, `Chunks[A] = Feed[Chunk[A]]`,
      2026-09-19). Its rule is that the pointer must name a commit that
      EXISTS on GitHub — origin has them all now — so the next bump will
      break its build at those seams. Not this repository's code; a
      reminder for whoever bumps: the migration pattern is in
      changelog.d/producer-writer-carrier-* and pwc-* (Writer.fold /
      Writer.collect / Source.concat for the drains, Writer.tell for
      the emits).
