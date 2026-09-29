# A fiber resumed by a foreign thread goes home

Status: WITHDRAWN, 2026-09-29 (resume-late-withdraw). Owner lane: `adaptive-elementwise-small-ring`.

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

## Correction: `fork`, not `forkLong` (2026-09-29, resume-late-small-ring-cost)

The landed hook sent a foreign answer home with `forkLong`, which wakes
a sleeping worker EVERY time. A `Source.zip` at capacity 7 resumes a
side every few elements, and it paid an unpark each: 3709 us an op, and
the zip's own lost-pairs race (a scope released by the collector,
source-zip-lost-pairs) met ~6x more often, the handoff's allocation
bringing collections forward. Measured on ONE build, the mode a
temporary property, two rounds each (ZipCapBenchmark, MergeCapBenchmark):

| handoff | merge cap 7 | merge cap 64 | zip cap 7 | zip cap 64 |
|---|---:|---:|---:|---:|
| `forkLong` (landed) | 271 | 73.6 | 3709 | 2558 |
| **`fork`** | **92** | **60.8** | 1383 | **370** |
| inline (before this spec) | 130 | 91.3 | 483 | 472 |
| Loom | 312 | 82.2 | 2396 | 793 |

`fork` wakes a worker only when none is awake, and wins three of four;
it is faster than Loom everywhere. The one loss is a tiny-ring zip
against inline (1383 vs 483): there the consumer running the producer
costs less than any handoff. Not special-cased — the drive cannot see a
ring's size, and the default capacity (64) is where `fork` wins.

## Withdrawn (2026-09-29, resume-late-withdraw)

`TestMergeOrder` "Channel.merge: each side arrives in exactly the order
it sent" went red on master (a sibling's gate; bisected with
`OKAY_MERGE_ROUNDS=400`: 5da64e406 green 400/400, f50932cbe red at round
9): a run of about a ring's capacity from one side arrived after the
next run. The mechanism is the buffer's, not the drive's:
`AdaptiveFifo.route()` is per THREAD (a ThreadLocal home part), so a
side's order holds only while its producer writes from one thread, and
this hook moved a resumed producer to another worker on every resume.
With `fork` instead of `forkLong` (ebdfd0ec7) the order law passed
5000/5000 rounds on master — rarer, not impossible — and a guarantee
does not ship on a rate. The hook is gone (`drive(null, x, k)` as
before); `resumeLate`, `resumeHere` and `DriveTask`'s `home` with it, the
foreign-thread law removed, the worker-inline law kept. The cap-64
elementwise merge is back to 1.12x Loom (90.9 vs 81.2, measured on this
code before the hook). THE WAY BACK: a route per PRODUCER, not per
thread — a merge's two feeds each own a part — which makes the order
independent of where a fiber resumes, and then the handoff can return
(backlog `channel-route-per-producer`). Note the inline resume moves a
producer too (worker to consumer thread), once per park; that path
passed 400/400 and is what the law has always run on.
