## okay-audit - the effect boundary checked at every build (specs/okay-audit.md)

- New JVM module, zero dependencies: a class-file scanner (JVMS §4
  constant pool + `ACC_NATIVE`) over each project's classes and
  third-party jars. A `business` project may reference none of
  `Boundary.Default` — network, files, console, sql, processes, time,
  randomness, threads, reflection, invoke, class loading, foreign,
  serialization, native methods; `handlers` and `runtime` projects are
  LISTED by provider, never failed. The listing is the DORA Art. 8
  dependency inventory. Carve-outs (compiler bootstraps, their types as
  bare classes, Scala 3's lazy-val VarHandle idiom) are printed in every
  report's header.
- sbt: `auditLayer` per project, root `audit` → `target/audit/report.{txt,json}`,
  fails on a finding. `TestAudit`: 12 tests on Java and Scala fixtures.
- Dogfood over 24 JVM projects + the Scala library, 53 inputs, 21 s: the
  first run found 232 real reaches and moved okay-codec, okay-bayes,
  okay-java and okay-data from business to handlers; okay-data's clock
  (`Hlc`) and random (`Uid`) are filed as design findings.
