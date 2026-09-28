## abandoned-lazylist-releases-nothing — a dropped `toLazyList` releases its merge once the collector finds it

`toLazyList` steps a program by its own `runWith` per element, so a
merge read that way and abandoned partway never ends: no drive, no
fiber handler sees an end, the scope entered in front is never
released, the feeder fibers stay parked on full channels for good —
every road, not one mechanism (merge-scopes-everywhere's open door).
Now a `CancelScope` registers itself with `Unreachable` at construction
— a `java.lang.ref.Cleaner` on the JVM, nothing on Native and JS — and
its release runs ONCE through whichever door comes first
(`CancelScope.Once`: the drive's end or cancel, the fiber's handler,
the collector). The action holds the once-cell and never the scope, so
the scope can be collected; a live program keeps its scope reachable
through its own rest. A backstop on the collector's clock, not a
deterministic stop: end the program (`runFoldUntil`) for one you can
time. Law: take 5 of a merged `toLazyList`, drop it, `System.gc()`
until the release is counted, on `Merge.Ready` and `Merge.Shared` —
red with the Cleaner door stubbed. specs/ready-merge.md (the stage),
docs/guide.md §6. Found on the way,
by a four-case probe: a scope reachable from its own cleaner is never
collected — `ReadyMerge`'s release closed over the run (so the
collector's door is now `onCollected`, the channels' close alone), and
a lambda in the class body captured `this` through its fields (so the
action is built by a helper from its parameters).

