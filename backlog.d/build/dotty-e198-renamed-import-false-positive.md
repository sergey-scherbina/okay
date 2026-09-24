- [ ] dotty-e198-renamed-import-false-positive — RECURRENCE LEDGER.
      `import okay.Row.{at as liftAt, plus}` warned "unused
      import" while `liftAt` was used as an extension method;
      deleting it failed with E008, which is the proof it was used.
      Dropping the RENAME compiles clean. Recorded rather than
      suppressed, because `scripts/gate.sh` is now red on warnings and
      the next person must not delete the line the compiler points at.
      Occurrences: 2026-09-11, src/test/scala/TestInstances.scala.
