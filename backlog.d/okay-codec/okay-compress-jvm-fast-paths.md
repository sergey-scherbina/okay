- [ ] okay-compress-jvm-fast-paths — okay-compress is one source for JVM,
      Scala.js and Native over byte arrays, byte by byte; aircompressor
      reads and copies eight bytes at a time through `Unsafe`, and stage 5
      measured the gap (history.d okay-compress): LZ4 decompress ~2x, ZSTD
      compress ~4.5x, ZSTD decompress ~2x behind on the JVM. A JVM-only
      `scala-jvm` object of 8-byte loads/stores and wild copies through
      `MethodHandles.byteArrayViewVarHandle` (JDK 9+, no `--add-opens`),
      with the same signatures implemented byte by byte for JS/Native,
      used in the three hot loops: LZ4's copies, ZSTD's `matchLength` and
      `BackBits`. Measure each change with CompressBench on a quiet box
      (the stage-5 numbers were taken at load 35–112). (2026-09-25)
