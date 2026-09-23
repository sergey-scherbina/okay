- [~] **okay-frege** — Frege (Haskell on the JVM) programs as okay programs,
      okay's effects lifted into Frege's IO (specs/frege.md): an sbt task
      compiling `.fr` (forked, `-target 17`); `Frege.stage` (await/tell) and
      `Frege.run[F]` (perform any operation of the row) over a Frege thread
      that is the continuation; one-shot refusal, Resource-scoped
      abandonment, lazy list <-> Chunks for pure data. Operator ask,
      2026-09-23.
