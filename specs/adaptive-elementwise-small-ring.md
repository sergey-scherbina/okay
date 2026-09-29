# A fiber resumed by a foreign thread goes home

Status: implemented, 2026-09-29. Owner lane: `adaptive-elementwise-small-ring`.

## Symptom

`MergeCapBenchmark.sourceMergeAtCapacity` at cap 64 (2 x 500 elements):
90.9 us on the `adaptive` default against Loom's 81.2 (1.12x, three
alternating rounds on master after spawnjoin-rise-bisect); parity at
cap 256 and 1024.

## Diagnosis (CPU profiles, both arms)

The consumer is the caller's own thread (here JMH's), outside the pool.
Each time it frees a slot of a full 64-slot ring it answers the waiting
producer's Await callback, and a callback drive resumes the fiber
INLINE on whoever answered (`Drive.op`: "after its first callback this
drive runs on whoever woke it"). So under `adaptive` the consumer spends
183 of its 595 samples (31%) in `Drive.drive` — running the PRODUCERS'
code — while the producers' workers sit parked. Under Loom the same
answer is an `unpark` (79 samples) and the producer runs on its own
carrier.

Inline resumption is right inside the pool: a worker that wakes another
fiber saves a wake by running it. It is wrong from a FOREIGN thread,
which then does the woken fiber's work instead of its own.

## The fix

- `Async.Drive` resumes a late answer through a hook, `resumeLate(x, k)`,
  whose default is today's inline `drive(null, x, k)` (JS, `drive(pool)`,
  every other drive unchanged).
- The JVM `DriveTask` of an `own`/`adaptive` scheduler overrides it: on
  one of our own workers (`ManagedWorker`) it resumes inline as before;
  on any other thread it hands the resumption to its home scheduler with
  `forkLong` (a worker claimed and woken at once), so the answering
  thread returns to its own work.

## Behaviour

- [x] LAW, red first: a fiber on `own` awaiting a callback that a FOREIGN
      thread answers resumes on a worker thread, not on the answering one
- [x] LAW: answered from inside a worker, it still resumes inline (on
      that worker)
- [x] the drive laws hold (TestAsync, TestSchedulerLaws, TestOwnMonitor,
      TestManagedBlocking) and the stream's merge laws (TestReadyMerge)
- [x] cap 64 on the default within 1.06x of Loom; cap 256/1024, the
      chunked lanes and spawnJoinSeq not worse (alternating arms)

## Results (2026-09-29)

Rows: src/jmh/history.d/2026-09-29T174904Z-adaptive-elementwise-small-ring.tsv. All through `jmh-lane.sh`.

- LAW red first: before the hook, the continuation of a fiber answered
  late by `foreign-answerer` ran on `foreign-answerer`; after it, on a
  `ManagedWorker`. Answered from a worker, it still runs inline there.
- TestAsync, TestSchedulerLaws, TestOwnMonitor, TestManagedBlocking:
  83/83. The stack-recursion inventory names the two new members of the
  drive/callback cycle (`resumeLate`, `resumeHere`) with `drive`'s bound.

| lane | default (3 rounds) | Loom (3 rounds) | ratio |
|---|---|---|---:|
| MergeCapBenchmark cap 64 | 73.8 / 74.0 / 75.8 | 81.0 / 81.1 / 82.1 | **0.92** (was 1.12) |
| cap 256 | 63.9 / 61.8 / 63.6 | 65.4 / 64.5 / 64.6 | 0.97 |
| cap 1024 | 62.8 / 63.9 / 61.7 | 60.6 / 61.0 / 61.2 | 1.03 |
| ChunkFlushBenchmark.okayChunked k=16 | 186.7 | 198.7 | 0.94 |
| okayChunkedShared k=16 | 199.9 | 202.2 | 0.99 |
| OwnMonitorBenchmark.spawnJoinSeq (own) | 66.9 | — | 64.4 in spawnjoin-rise-bisect |

The default is now FASTER than Loom at cap 64: the consumer does only
its own work, and a producer woken through `forkLong` gets a worker at
once.
