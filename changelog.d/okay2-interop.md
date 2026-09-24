## okay2-interop - cats, fs2 and zio for the Scala 2 core

Three subprojects inside the `okay2/` build (kyo is Scala 3 only, so
no okay2-kyo). The shape is the freer-monad one: a program is a tree,
and interop is interpreting it in the target monad by an `Into[R, M]`
(the natural transformation from the row's operations, composed along
`+` by the class test as `Handler.union` is).

- okay2-cats: `foldTo` into any cats `Monad` through `tailRecM`; the
  `Io` row whose operations ARE `IO` values (`Io.lift`, `Io.run`);
  `cats.free.Free` both ways; `okay2.cats.instances._` — every
  program row a `StackSafeMonad`, `Throws[E] + F` a `MonadError`
  (cats' `handleError` syntax reaches it).
- okay2-fs2: a Writer program as an `fs2.Stream[F, W]` with the other
  effects run between the elements, lazily (`take(1)` runs nothing
  past it, asserted); an `fs2.Stream[IO, W]` as a Writer program that
  pulls one element per `Io` operation.
- okay2-zio: `IntoZ` with an environment and an error type; `foldTo`
  into any ZIO; the `Zio` row; a Writer program as a `ZStream` by
  `unfoldZIO`; `fromZStream` by `runCollect` (a scoped pull waits for
  the Resource effect).
- 19 tests; 78 in the okay2 gate. Two more scalac-2 traps recorded in
  specs/okay2.md (one implicit section: `Remove.Aux`; instances need
  lexical scope). sbt: a root that aggregates a project depending on
  it is a lazy-val cycle — `LocalProject("okay2")` by name.

Docs: docs/okay2.md section 9, every snippet a test line.
