# okay-zio

- `ZioInterop.toZIO` (`ZIO.attemptBlocking(runWith)`) and `fromZIO`
  (the virtual thread parks on `unsafe.run`).
- `toZStream` — chunk for chunk via `ZStream.unfoldChunk` over the
  pure `Chunks.pull`; an infinite okay stream stays lazy on the ZIO
  side.
- `fromZStream` — the stream's scoped iterator, opened once and
  driven lazily (tested: pulling 32 elements builds at most 64); the
  scope closes when the iterator ends. Linear, like every external
  source.
- `ZioInterop.scheduler()` — okay fibers on the ZIO runtime
  (`unsafe.fork` + `attemptBlocking`; cancel = `interruptFork`).
