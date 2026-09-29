# Async

Asynchrony on every platform: callbacks, fibers, parallelism, races and
timeouts, with the same program on the JVM, Scala.js and Scala Native.

## Operations

| | |
|---|---|
| `async(x)` | a thunk run by the runtime, possibly blocking on the JVM and Native |
| `Async.await(register)` | the universal callback form: an error channel in, a canceller out (`await` answers only) |
| `Async.spawn(p)` | start a fiber; `join()` blocks for it, `joinAsync` waits as a program |
| `Async.par(a, b)` | both, in parallel |
| `Async.race(a, b)` | the first to finish; the other is cancelled |
| `Async.timeout(ms)(p)` | `Some(answer)`, or `None` after `ms` |
| `Async.sleep(ms)` | wait without holding a thread |
| `Async.attempt(p)` | a failure as a value |

## Running it

On the JVM and Native, `p.runWith` runs a program and blocks for its
answer; blocking is `CanBlock` evidence, which Scala.js does not have, so
there a blocking join is a compile error and `runAsync` drives the same
program through the event loop. A `Scheduler` decides where fibers run:
on the JVM an adaptive pool of owned workers by default, Loom a `given`
away ([schedulers](../schedulers.md)).

## Example

```scala
val prog: Int ! Async = async(20).flatMap(x => async(x + 22))
val answer = prog.runWith   // 42

val a = Async.spawn(async(20))
val b = Async.spawn(async(22))
val sum = a.join() + b.join()   // 42

val both = Async.par(async(1), async("one")).runWith   // (1, one)
val first = Async.race(Async.sleep(5_000).map(_ => "slow"), async("fast")).runWith   // fast
val late = Async.timeout(10)(Async.sleep(5_000).map(_ => 1)).runWith   // None
```

See also: `okay-async/src/main/scala/Async.scala`, the
[okay-async](../modules/okay-async.md) and
[okay-platform](../modules/okay-platform.md) modules,
[merge and wait](../merge-and-wait.md) for streams of async work.
