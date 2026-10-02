- [ ] fs2-effectful — okay-fs2 has two doors (`toFs2` of a PURE
      `Chunks`, `fromFs2` of a `Stream[IO, A]`); specs/interop.md also
      promised `Pipe ⇄ Stage`. Missing: an effectful okay source
      (`Unit ! Writer % W + Async`) as an fs2 stream, a `Stage` as a
      `Pipe` and back, and — after cats-effect-instances — an fs2
      `Stream[[X] =>> X ! Async, A]` compiled and run at okay's program
      monad itself, no `IO` in between. Cats-depth audit, 2026-10-02.
