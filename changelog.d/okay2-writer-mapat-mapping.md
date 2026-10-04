## okay2-writer-mapat-mapping - okay2's Writer.mapAt walks as an object per map: 8 B a tell less, stage 45's writerMap cost removed

The last half of okay2-split-at-rest-regressions. Operator: "Делай то что
нужно для okay2 только сразу всё" (do what okay2 needs, all of it at
once).

**The cost.** Stage 45 (`f1520259d`) moved `Writer.mapAt`'s walk into a
local `go` that closes over the `Split.at` extractor and `f`. So each
continuation it builds (`_ => go(k(()))`) carried one field more than the
old static recursion's. That is 8 B a tell, the +8 KB a run the stage-45
pricing read on writerMap.

**The fix** is the shape okay uses for `relay`. The walk is an object per
map (`Mapping`), so its continuations capture `this` and `k`.

**Measured,** three alternating rounds against master: 242 688 vs
250 664 B/op, which is the parent's bytes back, and 1.01x, 0.99x, 0.97x in
time (history.d okay2-writer-mapat-mapping).

**The item is closed.** produceFold's 1.09x was already answered: its
split is not the cost (okay2-split-at-rest-regressions).
