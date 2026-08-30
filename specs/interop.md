# P3 — Interop modules (okay-cats, okay-zio, okay-kyo, okay-fs2)

## Overview
Small bridge modules, one per ecosystem, so okay programs live inside
existing codebases and vice versa. Each module: typeclass instances
inward, value conversions outward, nothing more (module minimalism).

## okay-cats
- cats.Monad / MonadError / etc. instances for `[A] =>> A ! F` and Eff.
- Async ⇄ cats.effect.IO: run an okay program as an IO (suspend the
  handler loop) and lift an IO as an `Async` operation.
- Free ⇄ cats.free.Free conversion (both are freer encodings).

## okay-zio
- Async ⇄ ZIO (an okay program as a ZIO effect; a ZIO as an Await op).
- ZStream ⇄ Chunks (both chunked: chunk-for-chunk conversion, no
  re-buffering); Sink ⇄ Fold/Aggregator.

## okay-kyo
- `A ! F` ⇄ `A < S` where the rows correspond; Env/Emit/Choice ⇄
  Reader/Writer/Choice on programs that use the shared subset.

## okay-fs2
- fs2.Stream[F, A] ⇄ Chunks (uncons-based, both directions; fs2 chunks
  map to our Chunks losslessly); Pipe ⇄ Stage.

## Behavior
- [x] round-trips preserve results and (for streams) chunk boundaries
      where both sides are chunked
- [x] laziness survives conversion (an infinite okay stream converted
      to fs2/ZStream still pulls on demand, and back)
- [ ] instances pass the ecosystem's own law suites (cats-laws etc.,
      test scope only)

## Out of scope
- emulating foreign runtimes; anything beyond instances + conversions
