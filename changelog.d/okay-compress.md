## okay-compress - LZ4 and ZSTD of our own on JVM, Scala.js and Native; Arrow's compressed bodies

The operator: "Мы можем реализовать свою компрессию (LZ4/ZSTD)? ... Чтобы
работала на всех таргетах?", then "делай сразу весь этот лейн полностью".

- New cross module okay-compress (no dependency, byte arrays only):
  - `XxHash` (32 and 64);
  - `Lz4Block` and `Lz4Frame`, both ways;
  - `Zstd`, both ways: decompression of every RFC 8878 frame without a
    dictionary, and compression with hash-chain LZ77, repeat offsets,
    FSE tables of its own (the encoder derived from the decoder's table)
    and Huffman literals, at levels 1–9, default 3.
- okay-arrow reads and writes Arrow's LZ4_FRAME and ZSTD compressed IPC
  bodies (`OkayArrow.write(t, Some(codec))`).
- Checked against pyarrow on every codec, both ways. pyarrow's frames run
  as fixtures on Scala.js and Native, so real ZSTD and LZ4 decode on every
  platform.
- Measured against aircompressor (JMH): output at least as small (ZSTD
  161 vs 236 KB on 4 MiB of lines), speed 2–7x behind on the JVM. A
  JVM-only 8-byte fast path is filed: `okay-compress-jvm-fast-paths`.
- specs/okay-compress.md; docs/modules/okay-compress.md.
