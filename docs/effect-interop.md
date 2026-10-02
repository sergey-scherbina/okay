# Effect interop: ZIO, cats-effect and Future

An okay program is a value `A ! Row`: the answer `A` and the effects it
may perform. Most codebases that pick okay up already run on another
effect system — ZIO, cats-effect, or plain `Future`s — and none of them
is going away. This guide is how the two live side by side: every
direction a value can cross, what each door costs, what happens to an
error and to a cancellation on the way, and how to bring an effect type
of your own.

The short version:

- a foreign value inside an okay `direct` block is marked like any okay
  step — `Future(20).?`, `ZIO.attempt(22).?`, `IO(1).reflect`;
- every door waits by CALLBACK where it can, so no thread is parked
  while the other runtime works;
- cancellation crosses both ways, and finalizers run;
- ZIO's environment and typed error become okay effects, not strings in
  an exception.

Everything below is a test in the gate; the specs that designed it are
named at the end.

## The map

| from | to | door | waits by | a cancel on the far side |
|---|---|---|---|---|
| `Task[A]` | `A ! Async` | `ZioInterop.fromZIO`, `z.asOkay` (okay's) | callback | cancelling okay interrupts the fiber |
| `ZIO[R, E, A]` | `A ! ZioRow[R, E]` | `ZioInterop.fromZIOTyped` | callback | the same |
| `A ! Async` | `Task[A]` | `ZioInterop.toZIO`, `p.asZIO` | ZIO's blocking pool | — (runs to completion) |
| `A ! Async` | `Task[A]` | `ZioInterop.toZIOAsync` | callback | interrupting ZIO cancels the okay drive |
| `A ! ZioRow[R, E]` | `ZIO[R, E, A]` | `ZioInterop.toZIOTyped` | ZIO's blocking pool | — |
| `IO[A]` | `A ! Async` | `CatsInterop.fromIO`, `io.asOkay` | callback | cancelling okay cancels the IO |
| `A ! Async` | `IO[A]` | `CatsInterop.toIO`, `p.asIO` | cats' blocking pool | — |
| `A ! Async` | `IO[A]` | `CatsInterop.toIOAsync` | callback | cancelling the IO cancels the okay drive |
| `A < S` (kyo) | `A ! Async` | `KyoInterop.fromKyoAsync`, `k.asOkay` | kyo's run, blocking | — |
| `A ! Async` | `A < IO` (kyo) | `KyoInterop.toKyo`, `p.asKyo` | kyo's IO | — |
| `Future[A]` | `A ! Async` | a mark in a `direct` block, `f.asOkay` | callback | — (a Future cannot be cancelled) |
| any `M[A]` | `A ! G` | your `ForeignEffect[M]` | yours | yours |

Two doors run the okay program on a BLOCKING pool on purpose — see
"Blocking or callback" below.

## One expression across libraries

Functions written with cats, ZIO, kyo and okay compose in one chain, and
an okay function is called from inside each library's own code
(specs/interop-compose.md). One function from each:

```scala
val parse: String => IO[Int] = s => IO(s.trim.toInt)                          // cats
val double: Int => Task[Int] = i => ZIO.succeed(i * 2)                        // ZIO
val inc: Int => Int < (Abort[Nothing] & _root_.kyo.Async) = i => _root_.kyo.IO(i + 1) // kyo
val show: Int => String ! Async = i => async(s"<$i>")                         // okay
```

`asOkay` is okay's own extension (`import okay.asOkay`), one name for
every library: each interop module adds a `ToOkay` instance behind its
given import, chosen by the value's whole type, so kyo's `A < S` is found
as easily as `IO[A]`. On a function it gives an `A => B ! Async`, and
okay's `>=>` chains those:

```scala
val all: String => String ! Async = parse.asOkay >=> double.asOkay >=> inc.asOkay >=> show
```

The same four in one `direct` block. An IO and a ZIO are marked
directly. A kyo value crosses with `asOkay` first, because the mark
needs the `M[_]` shape and kyo puts the value first:

```scala
val p: String ! Async = direct {
val a = IO(10).?
val b = ZIO.succeed(10).?
val c = inc(20).asOkay.?
show(a + b + c).?
}
```

Going the other way, `asIO`, `asZIO` and `asKyo` turn an okay program
or an okay function into the other library's value, so its own
for-comprehension calls okay:

```scala
x <- parse("20")
y <- show(x).asIO
z <- IO(41).flatMap(show.asIO)
```

Everything crosses as `A ! Async`. Handle any other effect in an okay
row first (`Reader.run`, `runEither`). A typed `ZIO[R, E, A]` crosses
by the `direct` mark, as a `ZioRow` (below).

## ZIO

Module `okay-zio`; `import okay.zio.given` brings the instances, and
`ZioInterop` holds the functions.

### A ZIO inside an okay program

`fromZIO` forks the ZIO and waits for it as one `Async.await`. Under
`runWith` that parks the caller as any wait does; under the callback
runner (`Async.runAsync`, `Async.runAsyncCancellable`) nothing is
parked at all — the fiber's completion re-enters the okay drive:

```scala
assertEquals(fromZIO(ZIO.attempt(21).map(_ * 2)).runWith, 42)
val running = Async.runAsyncCancellable(fromZIO(z).map(_ * 2))
```

Cancelling that `running` interrupts the ZIO fiber: its `onInterrupt`
finalizers run, and a result arriving later resumes nothing. A ZIO
failure crosses as the same throwable; a defect is squashed to its
throwable too.

### An okay program as a ZIO

```scala
val z = toZIO(async(40).map(_ + 2))
```

`toZIO` runs the program on ZIO's blocking pool
(`ZIO.attemptBlocking`). `toZIOAsync` runs it by callback through
`ZIO.asyncInterrupt` instead — no thread is held while an okay `Await`
is pending, and interrupting the ZIO fiber cancels the okay drive. Which
one to pick is the section "Blocking or callback".

### The whole ZIO[R, E, A]

A `Task` has no environment and a `Throwable` error. A general ZIO has
both, and each lands on an okay effect of its own:

| ZIO | okay |
|---|---|
| environment `R` | `Reader % ZEnvironment[R]` |
| typed failure `E` | `Throws % E` |
| running, interruption, defects | `Async` |

`ZioRow[R, E]` is that row. `fromZIOTyped` reads the environment from
the Reader and raises a typed failure as `Throws`; `toZIOTyped` does the
reverse, taking the Reader from ZIO's environment:

```scala
type Row = Reader % ZEnvironment[Greeting] + Throws % String + Async
val z: ZIO[Greeting, String, String] = ZIO.serviceWith[Greeting](_.word + "!")
val p: String ! Row = Reader.ask[ZEnvironment[Greeting]].at[Row].map(_.get[Greeting].word)
assertEquals(exit(toZIOTyped(p).provideEnvironment(hi)), Exit.succeed("hi"))
val q: Int ! Row = raise[String, Int]("bad").at[Row]
assertEquals(exit(toZIOTyped(q).provideEnvironment(hi)), Exit.fail("bad"))
```

A defect stays a defect in both directions: a ZIO `die` fails the okay
`Async` run, never the `Throws`, and a throwable escaping an okay program
becomes a ZIO `die`, never `E`. That is the line ZIO draws itself between
the failures a signature promises and the ones it does not. The round
trip `toZIOTyped(fromZIOTyped(z))` answers as `z` for a success, a typed
failure and a read environment.

### ZIO written in okay's direct style

`import okay.zio.given` makes every `ZIO[R, E, _]` an okay `Monad`, so a
`direct` block can BE a ZIO:

```scala
val t: Task[Int] = direct[Task] {
  val a = ZIO.attempt(20).?
  val b = ZIO.attempt(22).reflect
  a + b
}
```

The block is an ordinary `Task`: nothing runs until ZIO runs it, a
failure skips the rest of the block, and the binds are ZIO's own
`flatMap`, so a hundred thousand of them nest safely on ZIO's
trampoline. It is the same idea as the `zio-direct` library
(`defer { x.run }`), written with okay's marks.

### ZIO values inside an okay block

The other way round: a block over an okay row binds ZIO values, each
landing on the narrowest row its type allows — a `Task` (or any ZIO with
no environment and a Throwable error) is one `Async` step, a typed error
adds `Throws % E`, an environment makes it the whole `ZioRow[R, E]`:

```scala
type Row = Throws % String + Async
val nope: ZIO[Any, String, Int] = ZIO.fail("nope")
val p: Int ! Row = direct {
  val a = nope.?
  a + 1
}
assertEquals(runEither[Int, Async, String](p).runWith, Left("nope"))
```

A ZIO whose effect the block's row cannot hold is a COMPILE error naming
the row: the same `nope.?` in a block over `Async` alone is refused,
because nothing there could answer its `Throws % String`.

## cats-effect

Module `okay-cats`; `import okay.cats.given`, and an `IORuntime` in
scope wherever an `IO` is run or marked.

`CatsInterop.fromIO` is an `Async.await` on the IO's
`unsafeToFutureCancelable`: no parked thread under the callback runner,
and cancelling the okay side cancels the IO, running its `onCancel`
finalizers. `toIO` runs an okay program as `IO.blocking`; `toIOAsync`
runs it by callback through `IO.async`, and cancelling the IO fiber
cancels the okay drive. Inside an okay block an `IO` takes all three
marks:

```scala
val p: Int ! Async = direct {
  val a = IO(20).?
  val b = IO.pure(2).reflect
  val c = !IO(20)
  a + b + c
}
```

The same module makes every okay program a lawful cats `Monad` (and a
`MonadError` over `Throws % E`), so cats syntax runs on okay programs —
see [okay-cats](modules/okay-cats.md).

## Future

`Future` is the standard library's, so its instance needs no import.
Marked in a block, a Future is one `Async` step that waits by callback:

```scala
val p: Int ! Async = direct {
  val a = Future(20).?
  val b = Future(2).reflect
  val c = !Future(20)
  a + b + c
}
```

A failed Future fails the program with the same throwable. A Future
cannot be cancelled, so cancelling the okay side only stops the program
from resuming on its result.

## Blocking or callback

Every direction INTO okay waits by callback. The two directions OUT of
okay each come as a pair, and the difference is one okay operation:

- `Async.await` (and everything built on it: channels, timers, the
  foreign doors above) never holds a thread while it waits;
- `Async.Run` (what `async(...)` builds) runs a thunk IN PLACE, and a
  thunk may block — a JDBC call, a file read, a `Thread.sleep`.

`toZIO` and `toIO` put the whole program on the other runtime's blocking
pool, which is right for any program. `toZIOAsync` and `toIOAsync` run
it by callback on whatever thread wakes it, which is right only when its
waits are `Await`s: an `Async.Run` that blocks there blocks one of ZIO's
or cats' COMPUTE threads. The short names `p.asZIO` / `z.asOkay` are the
always-safe pair (`toZIO`, `fromZIO`).

## Cancellation, precisely

- okay cancels (`Running.cancel()`, a timeout, a lost race): a pending
  foreign wait is unregistered, a ZIO fiber is interrupted, an IO is
  cancelled, their finalizers run; the okay future fails with
  `CancellationException`, and a foreign result arriving later resumes
  nothing.
- the foreign side cancels (ZIO interruption, IO cancellation) a
  program started by `toZIOAsync` / `toIOAsync`: the okay drive is
  cancelled, its pending `Await`'s canceller runs exactly once, and a
  later callback resumes nothing.
- the blocking doors (`toZIO`, `toIO`) run to completion: a thread
  running a blocking okay program is not interrupted.

## Your own effect

The `direct` macro knows no foreign library. A mark on a value that is
neither an okay program nor an operation of the block's row asks for a
`ForeignEffect[M]` for the value's type constructor, and binds what its
`lift` returns. An instance says two things: which okay effect the
value becomes (`type G`), and how to build the program. Here is Java's
`CompletableFuture`, with its failure unwrapped and its cancellation
carried:

```scala
given ForeignEffect[CompletableFuture] with
  type G[+X] = Async[X]
  def lift[A](m: CompletableFuture[A]): A ! Async =
    Async.await[A] { k =>
      val _ = m.whenComplete { (a, e) =>
        k(if e == null then Right(a) else Left(unwrap(e)))
      }
      () => { val _ = m.cancel(true) }
    }
```

and then it is marked like any other:

```scala
val p: Int ! Async = direct {
  val a = CompletableFuture.supplyAsync(() => 20).?
  val b = CompletableFuture.completedFuture(22).reflect
  a + b
}
```

For a type with several parameters the instance is for all but the
last fixed — okay-zio's are `ForeignEffect[[X] =>> ZIO[R, E, X]]` —
and when several instances match, the most specific wins, which is how
a `Task` lands on `Async` while a `ZIO[R, E, A]` lands on the whole
`ZioRow`. `G` is a type MEMBER rather than a second parameter because
the macro searches knowing only `M`. Put the instance where your users
will import it, beside the rest of your library's interop.

## Pitfalls

- **`!z` does not work on a ZIO.** ZIO declares its own `unary_!` — a
  deprecated negation of `ZIO[_, _, Boolean]` — and a class member
  always wins over an extension, so `!ZIO.attempt(2)` is ZIO's negation
  and does not compile. Write `z.?` or `z.reflect`. `IO` and `Future`
  have no such member.
- **Spell the row out for `.at`.** `.at[ZioRow[Greeting, String]]` finds
  no membership through the parameterised alias, while
  `.at[Reader % ZEnvironment[Greeting] + Throws % String + Async]`
  does. `ZioRow` is fine in a signature.
- **A ZIO's error type must match the row's `Throws`.** `Throws` is
  invariant in its error, so a `ZIO[R, Nothing, A]` — what
  `ZIO.serviceWith` infers — does not fit a row at `Throws % String`.
  Ascribe the ZIO's type.
- **One Reader per row.** The environment rides one
  `Reader % ZEnvironment[R]`; a ZIO environment of several services is
  one `ZEnvironment[Db & Log]`, not two Readers.
- **An IO needs an `IORuntime` at the mark**, as `unsafeRunSync` would.

## Streams and schedulers

Streams cross chunk for chunk: `ZioInterop.toZStream` / `fromZStream`
(and okay-fs2 for fs2), with chunk boundaries kept; a `ZStream` read
from okay is opened once, pulled lazily, and its scope closed when it
ends. `ZioInterop.scheduler()`
and `CatsInterop.scheduler` make the other runtime okay's `Scheduler`,
so okay fibers, `par`, `merge` and supervision run on it. See
[okay-zio](modules/okay-zio.md) and [okay-cats](modules/okay-cats.md).

## Further reading

- Andrzej Filinski, *Representing Monads*, POPL 1994 — monadic
  reflection: any monad written in direct style over delimited
  continuations. The `.?` mark on a foreign value is this construction.
- Oleg Kiselyov and Hiromi Ishii, *Freer Monads, More Extensible
  Effects*, Haskell Symposium 2015 — the open effect rows a foreign
  value is widened into.
- Gordon Plotkin and Matija Pretnar, *Handlers of Algebraic Effects*,
  ESOP 2009 — why `Reader` and `Throws` are handled, not thrown.
- ZIO: [interruption](https://zio.dev/reference/interruption/) and
  `ZIO.asyncInterrupt`; [zio-direct](https://github.com/zio/zio-direct).
- cats-effect: [the `Async` type class](https://typelevel.org/cats-effect/docs/typeclasses/async)
  (`IO.async` and its finalizer).
- In this repository: [direct style](direct-style.md) ("Foreign
  effects"), [the class ladder across cats, ZIO and kyo](interop-classes.md),
  and the specs `zio-async-bridge`, `zio-direct-cancel`,
  `zio-typed-row`, `direct-foreign-mark`, `cats-io-async`,
  `interop-compose`.
