## okay2-codec-stubs - Stubs, StubFiles, TsTypes and TsCheck in okay2-codec

The second lane of okay2/backlog.d/modules/okay2-codec-tooling (spec stage
58, docs/okay2.md section 33). `Stubs` writes the other side's
declarations from a Schema: a Python TypedDict module, a TypeScript
`.d.ts` in the JSON codec's shapes or in the wire's, the typed keys of
`JsonOptic.path`, and operations as signatures. `TsTypes` reads the data
subset of TypeScript declarations back into Scala 2 source and refuses
everything else by name and line. `StubFiles` and `TsCheck` (JVM) write
the declarations as a build step and ask a live `tsc` whether a
hand-written copy declares the same types.

What differs from Scala 3: `TsTypes` writes case classes and sealed
traits, each with its Schema in the companion. A plain alias is written
in place, because Scala 2 has no top-level type. okay2-codec's JVM
project gained a `scala-jvm` main source directory.

The ported suites are TestStubs, TestStubsPaths and TestTsTypes, whose
golden file the test build compiles, on JVM, Scala.js and Scala Native,
plus TestStubFiles on the JVM. TestTsCheck and TestStubsTsc are Live
(tsc).
