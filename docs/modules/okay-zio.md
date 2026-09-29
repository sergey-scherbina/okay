# okay-zio

> Async ⇄ ZIO by Loom, ZStream ⇄ Chunks chunk for chunk, and the ZIO
> runtime as an Okay Scheduler.

Depends on: `okay` (JVM), zio, zio-streams.

## Guide

**Values cross by parking or callbacks.** `toZIO` wraps a program as
`ZIO.attemptBlocking` — ZIO's blocking pool runs it and a virtual
thread parks wherever the program blocks. `fromZIO` forks the ZIO and
waits for it as ONE `Async.await`: the callback runner parks no thread,
`runWith` parks as for any Await, and cancelling the okay side interrupts
the ZIO fiber (its finalizers run; a late result resumes nothing).
Neither side simulates the other's runtime; each waits its own native
way. `p.asZIO` and `z.asOkay` are the same two doors as extensions.

**The whole `ZIO[R, E, A]`.** `Task` crosses with `fromZIO`/`toZIO`; a
ZIO with an environment and a typed error crosses with
`fromZIOTyped`/`toZIOTyped`, each channel onto its okay effect —
`ZioRow[R, E] = Reader % ZEnvironment[R] + Throws % E + Async`:

| ZIO | okay |
|---|---|
| environment `R` | `Reader % ZEnvironment[R]` — `Reader.run(env)` provides it |
| typed failure `E` | `Throws % E` — `raise(e)`, `runEither` |
| defect, interruption | the `Async` run fails, cancel interrupts |

```scala
type Row = Reader % ZEnvironment[Greeting] + Throws % String + Async
val z: ZIO[Greeting, String, String] = ZIO.serviceWith[Greeting](_.word + "!")
val p: String ! Row = Reader.ask[ZEnvironment[Greeting]].at[Row].map(_.get[Greeting].word)
val q: Int ! Row = raise[String, Int]("bad").at[Row]
```

`fromZIOTyped(z)` is a program over that row; `toZIOTyped(p)` is a
`ZIO[Greeting, String, String]` that reads `Greeting` from ZIO's
environment, and `toZIOTyped(q)` fails with `"bad"` in ZIO's typed
channel. A throwable escaping an okay program is a ZIO DEFECT, never a
typed failure — the same line ZIO draws itself. The round trip answers as
the original ZIO for a success, a typed failure and a read environment.

One spelling note: `.at[...]` needs the row written out, as above. Through
the alias, `.at[ZioRow[Greeting, String]]` finds no membership (the search
does not see through a parameterised alias), while `ZioRow` in a
signature is fine.

**ZIO in direct style.** `import okay.zio.given` makes any `ZIO[R, E, _]`
okay's `Monad`, so a `direct` block binds ZIO values with okay's own marks
— the block is a `Task`, nothing runs until ZIO runs it, a failure skips
the rest, and the binds are ZIO's own `flatMap` (stack-safe on its
trampoline):

```scala
import okay.Direct.*
import okay.zio.given

val t: Task[Int] = direct[Task] {
  val a = ZIO.attempt(20).?
  val b = ZIO.attempt(22).reflect
  a + b
}
```

**ZIO values inside a block over an okay program.** The same import
lets a `direct` block over an okay row bind ZIO values
(specs/direct-foreign-mark.md). Each ZIO lands on the narrowest row its
type allows — a `Task` (or any ZIO with no environment and a Throwable
error) is one `Async` operation, a typed error adds `Throws % E`, an
environment makes it the whole `ZioRow[R, E]`:

```scala
val p: Int ! Async = direct {
  val a = ZIO.attempt(20).?
  val b = ZIO.succeed(22).reflect
  a + b
}
```

A ZIO whose effect is not in the block's row is refused at compile time.

The prefix mark `!z` is the one spelling that cannot work here: ZIO
declares its own `unary_!` (deprecated negation of a `ZIO[_, _, Boolean]`),
and a class member always wins over an extension. Use `.?` or `.reflect`.
The design is Filinski's monadic reflection (*Representing Monads*, POPL
1994) — see [direct style](../direct-style.md); ZIO's own take on the
same idea is the separate `zio-direct` library (`defer { x.run }`).

`toZIOAsync` is the callback road for an `Async` program whose waits are
`Await`s: it runs no thread while an Await is pending, and ZIO interruption
calls the Await registration's canceller. It deliberately does not replace
`toZIO`: an `Async.Run` may block and belongs on ZIO's blocking executor.

**Streams cross chunk for chunk.** `toZStream` unfolds our pure
`Chunks.pull` with `ZStream.unfoldChunk` — chunk boundaries are
preserved and an infinite Okay stream stays lazy on the ZIO side.
`fromZStream` opens the stream's scoped iterator once and drives it
lazily (pulling 32 elements builds at most 64); the scope closes when
the iterator ends. Like every external source, the result is LINEAR —
consume it once.

**Their runtime, our fibers.** `ZioInterop.scheduler()` implements
Okay's `Scheduler` on the ZIO runtime: fork is `unsafe.fork` of an
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
| `ZioInterop.toZIOAsync` | `(=> A ! Async) => Task[A]` | callback drive; ZIO interruption cancels Await |
| `ZioInterop.fromZIO` | `(Task[A], runtime = default) => A ! Async` | an Await on a forked fiber; cancel interrupts it |
| `p.asZIO` / `z.asOkay` | extensions | `toZIO` / `fromZIO` |
| `ZioInterop.fromZIOTyped` | `(ZIO[R, E, A], runtime = default) => A ! ZioRow[R, E]` | environment from Reader, `E` as Throws |
| `ZioInterop.toZIOTyped` | `(=> A ! ZioRow[R, E]) => ZIO[R, E, A]` | Reader from the environment, raise as fail |
| `ZioRow[R, E]` | `Reader % ZEnvironment[R] + Throws % E + Async` | ZIO's three channels |
| `given zioMonad[R, E]` | `okay.Monad[[A] =>> ZIO[R, E, A]]` | `direct[Task] { z.? }` |
| `given zioForeign*` | `okay.ForeignEffect[[X] =>> ZIO[R, E, X]]` | `z.?` in a block over an okay row: `Async`, `Throws % E + Async` or `ZioRow[R, E]` |
| `ZioInterop.toZStream` | `Chunks[A] => ZStream[Any, Nothing, A]` | unfoldChunk over pure pull |
| `ZioInterop.fromZStream` | `ZStream[Any, Throwable, A] => Chunks[A]` | scoped iterator, lazy, linear |
| `ZioInterop.scheduler` | `(runtime = default) => okay.Scheduler` | ZIO runtime under Okay fibers |

## Gotchas

- `fromZStream` is consume-once (the scope belongs to the iterator);
  bridge to LazyList if you need re-observation.
- `import okay.given` is required for `runWith` and friends.

**Layers are modules** (`ZioLayers`, specs/di.md). `toLayer(m)` runs a
one-capability module under ZIO's `acquireRelease` — acquired when the
layer builds, released when ZIO's scope closes; `fromLayer(layer)` is
a module that builds the layer in a `Scope` of its own at acquisition
and closes it at release, so it composes with `and`; `fromEnvironment`
lifts a built `ZEnvironment` as a `Providing`. One capability per
conversion: their environment is typed by Tags per member, ours by a
context-function chain, and each side composes in its own words.
