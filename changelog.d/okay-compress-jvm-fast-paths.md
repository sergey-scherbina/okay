## okay-compress-jvm-fast-paths - 8-byte access per platform: 30-40% faster, LZ4 compression level with aircompressor

- `Mem` in `src/main/scala-{jvm,js,native}` of okay-compress: 8-byte
  loads, compares and copies. On the JVM it is a `VarHandle` byte-array
  view (JDK 9+, no `--add-opens`); on Scala.js and Native, bytes. The
  codecs stay one source and call it in their hot loops.
- On 4 MiB of lines, against aircompressor:
  - LZ4 compress 3.3 vs 3.4 ms;
  - LZ4 decompress 1.9 vs 0.9 ms (was 2.7);
  - ZSTD compress 22 vs 6.3 ms (was 30);
  - ZSTD decompress 11.1 vs 3.0 ms, each lane alone. The earlier
    "parity" there was noise.
- What is left is per-symbol work in ZSTD, filed with its leads:
  `okay-compress-zstd-speed`.
