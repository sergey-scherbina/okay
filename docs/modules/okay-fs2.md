# okay-fs2

> Streams cross CHUNK FOR CHUNK — both sides are chunked, nothing is
> re-buffered, and each side backpressures its own native way.

Depends on: `okay` (JVM), fs2-core (and cats-effect underneath).

## Guide

**Out: pure unfold.** `toFs2` unfolds our `Chunks.pull` with fs2's
`Stream.unfoldChunk` — a pure stream, chunk boundaries preserved
(4,4,2 stays 4,4,2), laziness crossing untouched: an infinite Okay
stream is an infinite fs2 stream that computes on pull.

**In: two runtimes, two waits.** `fromFs2` runs the fs2 stream on
its IO runtime, offering chunks into a bounded
`cats.effect.std.Queue`. `offer` SUSPENDS the IO fiber when the
queue is full — no thread blocks on their side; our side takes from
the queue by parking a virtual thread — Loom's way. Each runtime
waits natively, the bounded queue is the meeting point, and its
capacity is the backpressure window: an infinite fs2 stream under
`capacity = 2` is fine — take a little, the rest stays suspended.

## Tutorial

```scala
import okay.freer.*
import okay.freer.given
import okay.fs2.Fs2Interop

// okay chunks as a pure fs2 stream:
val s: fs2.Stream[fs2.Pure, Long] = Fs2Interop.toFs2(Chunks.range(0, 1000, 4))

// an fs2 stream (with IO inside) as okay chunks, backpressured:
val ch: Chunks[Int] = Fs2Interop.fromFs2(fs2.Stream.iterate(0)(_ + 1).covary[IO],
  capacity = 2)
Chunks.fold(Chunks.take(ch)(10))(using Fold.sum[Int])   // pulls only what it takes
```

## API reference

| member | signature | meaning |
|---|---|---|
| `Fs2Interop.toFs2` | `Chunks[A] => fs2.Stream[fs2.Pure, A]` | pure unfold, boundaries kept |
| `Fs2Interop.fromFs2` | `(fs2.Stream[IO, A], capacity)(using IORuntime) => Chunks[A]` | bounded-queue hand-off, linear |
| `Fs2Streams.toFs2` | `Source[W] => fs2.Stream[F, W]`, any `F: Async` | tells are elements; an interrupted stream cancels the pending `Await` |
| `Fs2Streams.fromFs2` | `(fs2.Stream[IO, A], capacity)(using IORuntime) => Source[A]` | parks no thread; cancelling the okay side cancels the fs2 fiber |
| `Fs2Streams.toPipe` | `Stage[I, O, Unit] => fs2.Pipe[F, I, O]` | pulls only what the stage asks for |
| `Fs2Streams.through` | `Source[I] => fs2.Pipe[IO, I, O] => Source[O]` | an fs2 pipe in an okay pipeline |

An fs2 stream also runs AT an okay program: `fs2.Stream[CatsEffect.Program, A]`
(okay-cats) compiles with `evalMap`/`parEvalMap` over okay programs and
no `IO` in its type:

```scala
.evalMap(i => CatsEffect.lift(okay.async(i * 2)))
.parEvalMap(4)(i => CatsEffect.lift(okay.async(i + 1)))
```

## Gotchas

- `fromFs2` is consume-once (a live queue underneath); bridge to
  LazyList for re-observation.
- The result of `fromFs2` recomputes NOTHING on retry — it is not a
  replayable source (and `retryChunks` will not accept it: the types
  refuse).
- `import okay.given` for `runWith` and the extensions, as in every
  satellite.
- A fs2 `Pipe` does not become a pull-driven `Stage`: the pipe owns its
  input stream. Use `Fs2Streams.through`, a pipe over a `Source`.
