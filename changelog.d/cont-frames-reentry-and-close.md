## cont-frames-reentry-and-close - a resumption from outside enters at the registers; every captured `k` ends in a `Kept`, `relink` deleted

- `Frames.Resume.apply` (a foreign handler's `k(x)`, forced) enters the
  machine directly at its registers (`Frames.machine(null, resume)`),
  without the `Bind(Return(a), k)` the loop's first step took apart:
  writerTellUnderDelim 1.155x -> 1.13x the single-list machine, with fewer
  bytes than it (270 vs 286 KB/op).
- A cut closes its `k` as a `Kept` (`Rev.close`), so every continuation a
  capture builds ends in one and `Rev.onto` resumes it typed; `relink`,
  `emptied` and the last two casts of `Rev` are deleted (a
  `dollarResumed` k, an `Enter` frame over a `Kept`, goes through
  `keptUnder`). Commit b332406d9; history.d and specs/freer-kont.md
  Results.
