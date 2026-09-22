## scala2-cont - okay.scala2 holds everything a Scala 2.13 program needs, under the library's own names

The operator's request (2026-09-23): one package for Scala 2, where
the continuation type is just `Cont`, not `Cont2`, and everything else
Scala 2 needs lives in the same package.

- `okay.scala2.Cont[A, S, R]`: `shift`, `reset`, `pure`, `map`,
  `flatMap`, `run(k)`, with answer-type modification. It wraps
  okay's own `Cont`, so it is just as stack-safe (100 000 binds,
  left- and right-nested, from 2.13).
- `okay.scala2.Eff[-R, A]`: several effects in one program. The row
  is an intersection of capability traits,
  `Eff[State[Int] with Writer[String], A]`. Each capability's
  companion holds its operations and its handler: `State`, `Reader`,
  `Writer`, `Throws`, `Async`. scalac 2.13 infers the rest of the
  row through each handler, and running a program with an effect
  still unhandled is a compile error (pinned with `compileErrors`).
  There is ONE cast, `Rows.coerce`, because on the Scala 2 side the
  row is only a phantom type; its reason is written beside it.
  `Eff.fromProg` and `Eff.toProg` convert to and from `Prog`.
- The 2.13 probe grows to 17 tests (`TestContFromScala2`,
  `TestEffFromScala2`), green under `-Xlint -Werror`.
- Docs: docs/modules/okay-scala2.md gains `Eff` and `Cont` sections
  (examples copied from the probe; Danvy-Filinski and Asai cited) and a
  list of what is not there yet. The guide paragraph and spec stage 2
  are updated.
