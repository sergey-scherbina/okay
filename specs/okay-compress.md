# okay-compress — LZ4 and ZSTD of our own, on every platform

Status: all five stages landed (2026-09-25). Asked by the operator: "Мы можем реализовать
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

- [x] Stage 1: xxHash32 and xxHash64 (the frames' checksums; the
      reference vectors). LZ4 block compress/decompress (a hash table
      of 4-byte sequences, greedy matching, the format's end rules:
      the last 5 bytes are literals, a match does not start in the last
      12). LZ4 frame (magic 184D2204, FLG/BD, header checksum, blocks
      with the "uncompressed" bit, end mark, content checksum;
      independent blocks of 4 MiB). Round trips on every platform,
      pyarrow both ways, a cut or damaged frame refused.
- [x] Stage 2: ZSTD DECOMPRESSION (RFC 8878): frame header (window,
      content size, checksum flag, no dictionary), raw / RLE /
      compressed blocks; literals (raw, RLE, Huffman with 1 or 4
      streams, treeless); sequences (FSE: predefined, RLE, compressed,
      repeat modes); execution with the three repeat offsets; the
      xxHash64 checksum; several frames and skippable frames.
      Everything pyarrow's zstd writes, at several levels, reads back.
- [x] Stage 3: ZSTD COMPRESSION: LZ77 matching (hash chain), sequences
      coded with FSE tables of our own, literals Huffman-coded when that
      is shorter, raw blocks when compression does not pay; pyarrow
      decompresses all of it; the ratio against pyarrow's own levels.
- [x] Stage 4: Arrow: `BodyCompression` read (LZ4_FRAME and ZSTD, per
      buffer: an int64 uncompressed length, -1 for a buffer left as it
      is) and written (`OkayArrow.write(t, compression)`); pyarrow's
      compressed IPC reads, ours validates in pyarrow.
- [x] Stage 5: the measurement: compress and decompress throughput
      against aircompressor on the JVM, the ratio beside it; docs.
      NOT done: timing on Native (it has no JMH; the round trips run there
      as tests only) — carried by okay-compress-jvm-fast-paths.

## Decisions

- **One source for three platforms, byte arrays only.** No `ByteBuffer`
  views, no `Unsafe`, no `VarHandle` in the shared code. That costs speed
  on the JVM (stage 5), and a JVM-only fast path is a separate,
  measurable item (`okay-compress-jvm-fast-paths`), not a fork of the
  codecs.
- **The FSE encoder is derived from the decoder's table.** For each symbol
  and each next state, the state it must come from is stored. The encoder
  cannot disagree with the decoder, and the tables zstd's reference
  encoder builds (`FSE_buildCTable`) are not reimplemented.
- **Levels** (`ZstdEncoder.compress(bytes, level)`), after stage 5
  measured the first cut. It searched 24 deep with lazy matching
  everywhere and ran 20x slower than aircompressor's level 3 for a
  smaller output. The default (3) is a 4-deep chain, with lazy matching
  only for a short match and the step growing through misses.

## Results

- Stages 1–4 (2026-09-25):
  - `TestLz4` and `TestZstd` run on JVM, Scala.js and Native: every
    sample, a frame past 4 MiB, cuts and flipped bytes refused, and
    pyarrow's own frames as fixtures, so real ZSTD (levels 3 and 19) and
    LZ4 decode on every platform.
  - Live, against pyarrow: `TestLz4PyArrow` (frames and raw blocks both
    ways) and `TestZstdPyArrow` (pyarrow's levels 1, 3, 9 and 19 read
    here; ours read there; the ratio printed beside pyarrow's level 3).
  - Arrow: `TestOkayArrow` (compressed bodies on every platform) and
    `TestPyArrowOracle` (pyarrow's LZ4 and ZSTD IPC read here; ours
    validate there).
  - Found on the way:
    - `Lz4Block.compress` answered an END POSITION where the frame needed
      a length. A block test at offset 0 could not tell the two apart;
      the test now writes at an offset.
    - The Huffman header could claim four streams for one.
    - An LZ4 table expectation was wrong: sequential int64s halve under
      pyarrow's LZ4 too (160000 -> 80069).
  - Mutants, each red:
    - an LZ4 overlapping match copied with arraycopy;
    - ZSTD's repeat-offset shift when the literal length is 0 ignored
      (pyarrow's fixtures red);
    - the encoder's extra bits in the wrong order.
- Stage 5 (`CompressBench`, JMH, load 35–112; paired lanes, interop
  checked in setup):

  | 4 MiB, JMH, `lines` / `text` | okay-compress | aircompressor 2.0.3 |
  |---|---|---|
  | LZ4 compress (block) | 4.9 / 5.2 ms | 5.7 / 3.6 ms |
  | LZ4 decompress (block) | 2.7 / 3.6 ms | 1.3 / 0.9 ms |
  | ZSTD compress (level 3) | 30 / 15 ms | 6.6 / 2.1 ms |
  | ZSTD decompress | 7.1 / 5.2 ms | 3.6 / 0.8 ms |
  | LZ4 output | 1 021 339 / 1 746 074 bytes | 1 029 432 / 1 929 484 bytes |
  | ZSTD output | 161 076 / 167 633 bytes | 236 268 / 170 999 bytes |

  Before the optimisation pass the same bench read LZ4 decompress 5–10x
  behind (a closure-captured position boxed into an `IntRef` in the hot
  loop) and ZSTD compress 20x behind (24-deep chain with lazy matching
  everywhere); both are recorded in history.d.
