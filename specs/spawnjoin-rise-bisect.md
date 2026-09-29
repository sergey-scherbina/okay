# The slice hooks' price: own spawn/join 1.7x, found and paid back

Status: implemented, 2026-09-29. Owner lane: `spawnjoin-rise-bisect`.
Found by adaptive-chunked-merge-cost's regression check.

## Symptom

`OwnMonitorBenchmark.spawnJoinSeq` (own, monitor 100us; 1 000 sequential
spawn + join) read ~110 us on master against ~64 before — the lane where
`own` beats kyo 2.3x on five-way sequential spawn/join.

## Diagnosis

`git bisect run` over the core, okay-async, okay-platform and build
commits since 104f5f360, one `jmh-lane.sh` run a step (< 92 good, > 96
bad): first bad **cda0a94b5** (drive-interrupts-blocking-run +
drive-resume-throw-lost), 63.8 on its parent f2d0a98a7 against 111.1 on
it. That commit made a cancel interrupt the thread running a drive's code
(a real defect, with laws in TestAsync) and paid for it on EVERY slice of
every fiber: a ThreadLocal get and set to find a nested drive, a monitor
enter/exit on the way out (and in `suspend`/`resume` for a nested
slice), and a `Bind` built around every late resumption. A spawn/join
runs about two slices, one nested: ~47 ns an iteration, ~47 us a run.

## The fix — same behaviour, cheaper protocol

- [x] the running drive is a plain field of our own worker thread
      (`ManagedWorker.drive`); the ThreadLocal stays for foreign and
      virtual threads — only the owning thread reads or writes it
- [x] no monitor on a slice's way out: a volatile handshake with
      `cancel` (it writes `stopped` then reads `runner`; the slice writes
      `runner = null` then reads `stopped`), and a cancelled slice waits
      out `cancel`'s critical section with an empty `synchronized` before
      it takes the interrupt back, so an interrupt never outlives the
      drive on a pooled worker; `resume` delivers a cancel it sees itself
- [x] a late answer resumes inside the drive's loop (`drive(null, x, k)`)
      rather than through `Free.Return(x).flatMap(k)`: `k(x)` still runs
      inside the try and the slice, as drive-resume-throw-lost requires
- [x] the laws that made cda0a94b5 hold: TestAsync, TestSchedulerLaws,
      TestOwnMonitor, TestManagedBlocking — 81/81

## Results (2026-09-29, one session, arms rotating over three rounds)

| build | spawnJoinSeq (us) |
|---|---|
| f2d0a98a7 (before cda0a94b5) | 68.9 / 62.8 / 64.2 |
| master (c3e26a09a) | 110.6 / 105.6 / 111.6 |
| this lane | **64.2 / 64.6 / 64.4** |

Step by step against master, same session: the worker field alone
111.2/113.5 -> 95.3/97.8; with the handshake 109.2/110.3 -> 71.2/80.0;
with the Bind gone, the table above.
Rows: src/jmh/history.d/2026-09-29T170004Z-spawnjoin-rise-bisect.tsv.
