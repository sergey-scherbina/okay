## okay2-persist-file - the file engine, Segments, Doctor, Configs and Streams in okay2-persist

Lane 1 of okay2-persist-rest (operator: "do everything needed for okay2,
all at once"; specs/okay2.md stage 56, docs/okay2.md section 37).

- `FileStore` (JVM): okay-persist's segment engine in Scala 2, with the
  same on-disk bytes, so the Scala 3 engine and this one read each
  other's files. Recovery cuts off a torn tail. A segment in a newer
  format is refused and the error names the file. A v1 active segment is
  closed and a v2 one rolled. Retention drops whole segments, compaction
  replaces them by atomic rename, and a second handle on the directory
  sees appends, rolled segments and deleted ones.
- `Segments.parse` and `Doctor.scan`, the two independent readers of
  that format.
- `Configs` and `Streams` (`stream`, `tail`, `chunks` over okay2-stream's
  `Source`), on JVM, Scala.js and Scala Native. okay2-persist now
  depends on okay2-stream.
- Suites: TestFileStore (the StoreSuite contract on files, plus 13 crash
  and two-handle cases), TestFileStoreRace (the 24-opener law tagged
  `Live`, as okay-persist tags it), TestDoctor, TestConfigs, TestStreams.
- Not ported: `Repair`, which needs okay's `Condition`.
