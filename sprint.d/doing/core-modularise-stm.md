- core-modularise-stm — stage 5 of specs/core-modules.md, the last
  filed seam, and the spec had it wrong. `Providing.Facts` "backed by
  `TMap`" was a naming coincidence: `TMap` is a TYPED map, not a
  transactional one. The real edge was in a platform directory, and it
  decided the cut — `TRef.modify` is the one-cell transaction and needs
  no runtime, so `TRef` stays in the core and only the multi-cell
  machinery leaves. One `dependsOn` in the whole family, test-only.
