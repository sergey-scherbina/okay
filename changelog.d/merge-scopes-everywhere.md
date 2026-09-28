## merge-scopes-everywhere — cancel scopes on the blocking schedulers too; a stopped merge ends its producers

Cancel scopes reached only the callback drive (own, adaptive, JS). Now a
fiber on Loom, `forkJoin` or `threads` runs its program through
`Async.runFiber`, a handler of the fiber's own that keeps the scopes the
program opens and releases every one still open however it leaves — so an
early stop or a cancel releases a `mergeReady`'s parked sources on the
default scheduler as well. And `Source.merge`'s release now CLOSES its
sides' channels (every `Merge.Ready` join; `Merge.Shared`'s chunked
joins via `Source.releasing`, its element join not): a merge that is cancelled or stopped early (`take`,
`runFoldUntil`) ends its feeder fibers, where each used to fill its buffer
and park for good. Laws in TestReadyMerge on Loom and own, each watched
red under its mutant. The first cut kept the scopes in a ThreadLocal and
paid +152 B on every Loom fork (7.49 vs 5.97 MB per 10 000 fork/joins);
the fiber's own handler measures at parity. A bare `runWith` on a
caller's thread is the one road still without scopes. specs/ready-merge.md.
