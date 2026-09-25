# okay-compress — LZ4 and ZSTD of our own, on every platform

Status: open (2026-09-25). Asked by the operator: "Мы можем реализовать
свою компрессию Compression (LZ4/ZSTD)? Это сложно? Можем сделать ее
эффективной? Чтобы работала на всех таргетах?" — then "Можешь делать
сразу весь этот лейн полностью."

## Why

Arrow's IPC body compression is LZ4_FRAME or ZSTD, and nothing else
(specs/okay-arrow.md): without them okay reads no compressed stream a
Python or Parquet producer writes, and writes none. Neither codec is in
the JDK, in Scala.js or in Scala Native's standard library. The JVM has
native bindings (lz4-java, zstd-jni) and a pure-Java port
(aircompressor, which uses `Unsafe`); none runs on JS or Native.

## The design

- `okay-compress`, a cross module (JVM, Scala.js, Native) with NO
  dependency, over `Array[Byte]` only: no `ByteBuffer`, no `Unsafe`, no
  native code. Hot loops allocate nothing.
- A `Codec` facade: `name`, `compress(bytes)`, `decompress(bytes)`;
  `Lz4Frame`, `Lz4Block`, `Zstd`. A stream cut short, a bad checksum, a
  frame feature not implemented is refused BY NAME, never answered with
  wrong bytes.
- The oracle is pyarrow (`pa.compress`/`pa.decompress`: codecs `lz4`
  = the frame format, `lz4_raw` = a block, `zstd`), both ways.
- The reference for speed is aircompressor (pure Java, test-only
  dependency), measured with JMH.

## Stages

- [ ] Stage 1: xxHash32 and xxHash64 (the frames' checksums; the
      reference vectors). LZ4 block compress/decompress (a hash table
      of 4-byte sequences, greedy matching, the format's end rules:
      the last 5 bytes are literals, a match does not start in the last
      12). LZ4 frame (magic 184D2204, FLG/BD, header checksum, blocks
      with the "uncompressed" bit, end mark, content checksum;
      independent blocks of 4 MiB). Round trips on every platform,
      pyarrow both ways, a cut or damaged frame refused.
- [ ] Stage 2: ZSTD DECOMPRESSION (RFC 8878): frame header (window,
      content size, checksum flag, no dictionary), raw / RLE /
      compressed blocks; literals (raw, RLE, Huffman with 1 or 4
      streams, treeless); sequences (FSE: predefined, RLE, compressed,
      repeat modes); execution with the three repeat offsets; the
      xxHash64 checksum; several frames and skippable frames.
      Everything pyarrow's zstd writes, at several levels, reads back.
- [ ] Stage 3: ZSTD COMPRESSION: LZ77 matching (hash chain), sequences
      coded with FSE tables of our own, literals Huffman-coded when that
      is shorter, raw blocks when compression does not pay; pyarrow
      decompresses all of it; the ratio against pyarrow's own levels.
- [ ] Stage 4: Arrow: `BodyCompression` read (LZ4_FRAME and ZSTD, per
      buffer: an int64 uncompressed length, -1 for a buffer left as it
      is) and written (`OkayArrow.write(t, compression)`); pyarrow's
      compressed IPC reads, ours validates in pyarrow.
- [ ] Stage 5: the measurement: compress and decompress throughput
      against aircompressor on the JVM, the ratio beside it, and the
      same round trip timed on Native; docs.

## Decisions

## Results
