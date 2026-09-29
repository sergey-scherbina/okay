- [ ] zio-typed-row — the whole `ZIO[R, E, A]`, not only `Task[A]`:
      typed failure `E` <-> `Throws % E`, environment `R` <->
      `Reader % ZEnvironment[R]` (or the DI capability ZioLayers already
      reaches), so `ZIO[R, E, A]` <-> `A ! Reader % ZEnvironment[R] +
      Throws % E + Async` both ways. Follows zio-direct-cancel.
