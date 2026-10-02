## okay2-forklong — okay2's schedulers take a long fork at once

The Scala 2.13 twin gets `Scheduler.forkLong` (default `fork`), and its
`own`/`adaptive` override it: the fork, then one sleeping worker claimed
(`Worker.waking`) and woken. The chunked feeds (`chunkedMerge`,
`bufferChunked`) fork with it. okay2's `own` has no monitor, so the case
it fixes is starker than in okay: a second long fiber forked from outside
waited for the first to END. `TestForkLong` red first (2 s waited), then
green; okay2Platform laws green. specs/adaptive-chunked-merge-cost.md.
