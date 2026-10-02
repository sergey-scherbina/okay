# fs2-effectful — effectful okay sources and stages in fs2, fs2 at an okay program

Status: done, 2026-10-02. Owner lane: `fs2-effectful`. From the
cats-depth audit (backlog okay-cats); specs/interop.md promised
`Pipe ⇄ Stage`, and okay-fs2 had only a PURE `Chunks` out and an IO
stream in.

## Interface (object `Fs2Streams`, okay-fs2)

```scala
def toFs2[F[_]: Async, W](s: Source[W]): fs2.Stream[F, W]         // any cats-effect F
def fromFs2[A](s: fs2.Stream[IO, A], capacity: Int = 64)(using IORuntime): Source[A]
def toPipe[F[_], I, O](st: Stage[I, O, Unit]): fs2.Pipe[F, I, O]
def through[I, O](src: Source[I])(pipe: fs2.Pipe[IO, I, O])(using IORuntime): Source[O]
```

## Behavior

- [x] a `Source` with `async` steps between tells is an fs2 stream, in
      order; its pending `Await` is cancelled (canceller run) when the
      fs2 stream is interrupted
- [x] an fs2 IO stream is a `Source` that parks no thread (an `Await`
      on the queue's take), and its fs2 fiber is cancelled when the okay
      side is cancelled
- [x] a `Stage` is a `Pipe`, pulling input only as it asks (an infinite
      input, a stage that stops after three)
- [x] an fs2 `Pipe` applied to an okay `Source`, back to a `Source`
- [x] an fs2 stream compiled AT `CatsEffect.Program`, with `evalMap` of
      okay programs and `parEvalMap`, no IO in its type

## Decisions

- **A Pipe is not turned into a pull-driven `Stage`.** A `Stage` asks
  for each input; an fs2 pipe owns its input stream and pulls it
  itself. Inverting that needs a second fiber and two queues between
  them; what a user needs — an fs2 pipe in an okay pipeline — is
  `through`, a pipe over a `Source`.

## Results

- TestFs2Effectful, 8 tests, green: async steps in order; an
  interrupted fs2 stream runs the parked `Await`'s canceller; 1000
  elements through a capacity-4 hand-off; cancelling the okay side runs
  the fs2 stream's finalizer; a running-sum stage over an infinite input
  stops after three; 100 000 elements through `Stage.id` as a pipe (the
  inventory row's evidence); a pipe over a `Source`; an fs2 stream with
  `parEvalMap` compiled at `CatsEffect.Program`.
- Mutant: a `CancelScope` that does not cancel the fs2 fiber fails the
  cancellation test (5 s wait, finalizer never ran).
