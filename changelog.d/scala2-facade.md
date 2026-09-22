## scala2-facade - okay from Scala 2.13, through a facade the 2.13 compiler can read

The operator asked for okay to compile under both Scala 3.9 and 2.13.
The spec measures why a cross-build means rewriting the core: the
effect row is a union type, there are 182 `inline def`s in `src/main`,
plus opaque types and macros. Consuming the Scala 3 jars directly from
2.13 fails at the first call (`Unsupported Scala 3 inline method
flatMap; found in class okay.Free`). Given those measurements, the
operator chose the facade.

- okay-scala2 (new, JVM, Scala 3): `Prog[A]`, a program over
  `Async + Throws % Throwable` with plain `map`/`flatMap`/`attempt`/
  `recover`/`run()`/`runEither()` and `Prog.pure/delay/fail/
  fromEither/sequence`. Scala 3 callers cross through
  `Bridge.lift`/`Bridge.program`. The program lives in `Body`, a value
  class, because scalac 2.13 refuses a class whose CONSTRUCTOR names
  the union row. This was bisected over four spellings. Methods are
  read lazily and are not affected.
- okay-scala2-probe (new, Scala 2.13.18, `-Ytasty-reader -Xlint
  -Werror`): 7 suites written in Scala 2 that call every public
  facade method, compiled by scalac 2.13 in the ordinary gate.
- The classpath needs two stdlibs, in a fixed order: 2.13.18 first
  and 3.9.0 behind it, for compile and for run. sbt refuses the
  transitive 3.9 jar under a 2.13 compiler (SIP-51), so it is
  excluded, resolved in a hidden configuration and appended. The
  probe uses the same settings docs/modules/okay-scala2.md gives
  users.
- Docs: docs/modules/okay-scala2.md (setup, example copied from the
  probe, why), a guide section, the module index entry.
  specs/scala2-facade.md has stage 0 by hand, stage 1, two refuted
  hypotheses, and the later stages.
