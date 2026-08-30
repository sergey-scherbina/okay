# okay-zio

> Async ⇄ ZIO by Loom, ZStream ⇄ Chunks chunk for chunk, and the ZIO
> runtime as an okay Scheduler.

Depends on: `okay` (JVM), zio, zio-streams.

## Guide

**Values cross by parking.** `toZIO` wraps a program as
`ZIO.attemptBlocking` — ZIO's blocking pool runs it and a virtual
thread parks wherever the program blocks. `fromZIO` runs a ZIO to
completion inside ONE okay async operation (`unsafe.run` — again, a
parked virtual thread). Neither side simulates the other's runtime;
each waits its own native way.

**Streams cross chunk for chunk.** `toZStream` unfolds our pure
`Chunks.pull` with `ZStream.unfoldChunk` — chunk boundaries are
preserved and an infinite okay stream stays lazy on the ZIO side.
`fromZStream` opens the stream's scoped iterator once and drives it
lazily (pulling 32 elements builds at most 64); the scope closes when
the iterator ends. Like every external source, the result is LINEAR —
consume it once.

**Their runtime, our fibers.** `ZioInterop.scheduler()` implements
okay's `Scheduler` on the ZIO runtime: fork is `unsafe.fork` of an
`attemptBlocking`, completion rides `fiber.await`, cancel is
`interruptFork`. One `given`, and `spawn`, `parMap`, `merge`,
supervision — everything fiber-shaped — runs on ZIO.

## Tutorial

```scala
import okay.given
import okay.zio.ZioInterop

// okay program as a ZIO Task:
val t: Task[Int] = ZioInterop.toZIO(async(blockingWork()))

// a ZIO inside an okay program:
val p: Int ! Async = ZioInterop.fromZIO(ZIO.succeed(41).map(_ + 1))

// chunked streams across, boundaries kept:
val zs: ZStream[Any, Nothing, Long] = ZioInterop.toZStream(Chunks.range(0, 1000))
val ch: Chunks[Long] = ZioInterop.fromZStream(zs)   // linear!

// okay fibers on the ZIO runtime:
given okay.Scheduler = ZioInterop.scheduler()
Async.par(async(1), async(2)).runWith
```

## API reference

| member | signature | meaning |
|---|---|---|
| `ZioInterop.toZIO` | `(=> A ! Async) => Task[A]` | run as attemptBlocking |
| `ZioInterop.fromZIO` | `(Task[A], runtime = default) => A ! Async` | a ZIO as one async op |
| `ZioInterop.toZStream` | `Chunks[A] => ZStream[Any, Nothing, A]` | unfoldChunk over pure pull |
| `ZioInterop.fromZStream` | `ZStream[Any, Throwable, A] => Chunks[A]` | scoped iterator, lazy, linear |
| `ZioInterop.scheduler` | `(runtime = default) => okay.Scheduler` | ZIO runtime under okay fibers |

## Gotchas

- `fromZStream` is consume-once (the scope belongs to the iterator);
  bridge to LazyList if you need re-observation.
- `import okay.given` is required for `runWith` and friends.
