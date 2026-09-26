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

- [ ] Stage 6: SNAPPY (compress-snappy, 2026-09-26), the RAW format
      (a varint length, literals and copies with 1-, 2- and 4-byte
      offsets — no framing): what Parquet pages use. Ours in the facade
      as `Compression.snappy` (greedy 4-byte hash matching; the decoder
      refuses a copy before the start, a length past the end, a cut
      element), aircompressor's `SnappyRaw*` behind `Aircompressor.given`;
      every sample round-trips, each reads the other's output, a damaged
      block is refused as `Corrupt`.

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

- okay-compress-jvm-fast-paths (2026-09-25): `Mem` per platform
  (`src/main/scala-{jvm,js,native}`). The JVM's is a `VarHandle` byte-array
  view; Scala.js and Native keep bytes. The shared codecs call it in
  LZ4's match extension, copies and short literals (two 8-byte copies
  inside checked limits), and in ZSTD's `matchLength`, match copies and
  one 8-byte load in `BackBits`. Same tests on all three platforms, and
  pyarrow both ways.
  - CompressBench, 4 MiB of lines, ours vs aircompressor:
    - LZ4 compress 3.30 vs 3.41 ms (was 4.90 vs 5.72);
    - LZ4 decompress 1.95 vs 0.94 (was 2.69 vs 1.30);
    - ZSTD compress 21.8 vs 6.3 (was 30.1 vs 6.6);
    - ZSTD decompress, each lane ALONE, 11.1 vs 3.0.
  - The stage-5 first run's ZSTD decompression "parity" (9.7 vs 10.1) was
    noise, its errors wider than the gap. The rest of the gap is
    `okay-compress-zstd-speed`.

- okay-compress-zstd-ratio (2026-09-25): the 1.3–1.6x gap to pyarrow on
  Arrow tables was measured buffer by buffer. The int64 and string-data
  buffers were level. The OFFSET buffers (increasing int32s) went RAW:
  their low bytes span 0–255, and `HuffmanEncoder` wrote only the direct
  weight form, which holds 128 weights. The first hypothesis, "the int64
  column", was REFUTED: it compressed to exactly pyarrow's 20 354 bytes.
  - FSE-coded Huffman weights (RFC 8878 4.2.1.2): two interleaved states
    over one backward stream. The chain that emits the next-to-last weight
    ends on a state whose update READS bits (the decoder stops on reading
    past the start), and the encoder is derived from the decoder's table
    as the sequence encoder is. Validated by pyarrow on a new
    "int32 offsets" sample: 7 452 vs its 7 447 bytes.
  - The head table is sized to the input; it was 2^17 entries (512 KiB)
    per call, most of a small buffer's cost.
  - Mutant: the two initial states written in swapped order. The round
    trip went red ("Huffman weights that do not complete a tree").
  - Arrow+ZSTD at 1 000 rows: bytes and time in specs/okay-arrow.md
    (stage 7a); history.d okay-compress-zstd-ratio.


- okay-compress-zstd-speed (2026-09-25): ZSTD on 4 MiB of lines against
  aircompressor, each lane through `scripts/jmh-lane.sh`:
  - decompress 11.1 -> 4.28 ms (air 2.29; the gap 3.7x -> 1.9x);
  - compress 21.8 -> 16.5 ms (air 6.45; 3.5x -> 2.6x).
  - What landed, in the order measured:
    - short-offset match copies by whole periods of 8+ bytes (a period
      under 8 repeats, so a copy from `p` bytes back is the same
      pattern): 11.1 -> 5.6 ms. Every period 1–9 at every length is
      tested; mutant `p = offset` went red;
    - the encoder's literal/match length codes by table and high bit,
      where it searched the base table linearly;
    - XXH64/XXH32 lanes through `Mem`: the checksum read eight bytes
      with `Le`, one byte at a time, and was 13% of decompression;
    - the sequence loop keeps the output buffer, its position and the
      FSE symbol tables in locals. Room is taken once per block, and a
      damaged length is refused against the block's 128 KiB. New test:
      a damaged frame fails as `Corrupt`, never as an index error.
      Mutant without the check: 84 of ~1 500 damaged frames ran off
      the buffer;
    - with the two above: 5.40 -> 4.28 ms;
    - the encoder's per-block tables without boxing. `Array.tabulate`
      takes a `ClassTag` and boxes every Int: 9% of compression.
  - REFUTED:
    - wild copies (whole 8-byte words into reserved room, as
      aircompressor's copyMatchTail) were level with the period fix
      (5.54 vs 5.58 ms) and were reverted;
    - the `BackBits` register window gave ~2%, inside the noise. It was
      kept as the shape the reference decoder has.
  - THE INSTRUMENT WAS WRONG FIRST. JMH's `-prof stack` samples at
    safepoints, which sit on loop back-edges. It put 39–42% of
    decompression in `Mem.copyMatch`. async-profiler, with no safepoint
    bias, put it at 16%, and the flat profile after the fixes reads:
    - the sequence loop 31%;
    - `copyMatch` 20%;
    - FSE `next` plus `BackBits` 31%;
    - `xxh64` 5%.
  - Compression after the lane: `Matcher.best` 22%, `insertUpTo` 13%,
    `BitWriter.write` 10%. That is the hash-chain search itself;
    aircompressor's level 3 is "double fast" with no chains. Filed as
    `okay-compress-zstd-speed-2` with the FSE and Native leads;
    history.d okay-compress-zstd-speed.

- okay-compress-zstd-speed-2 (2026-09-25, PLAN): compression's time is
  the chain search (`Matcher.best` 22%, `insertUpTo` 13%, from
  async-profiler). Reference zstd and aircompressor use "double fast" at
  levels 1 to 3 (`ZSTD_compressBlock_doubleFast`):
  - an 8-byte hash table for long matches and a 5-byte one for short
    matches;
  - one probe each and no chain;
  - a repeat offset tried at `ip+1`, and matches extended backward;
  - a step that grows through input that does not match;
  - the tables filled at a few positions inside a match, not at every
    byte.
  Levels 1 to 3 (the default is 3) move to it, and the chain stays for
  higher levels.
  - [x] every sample round-trips at levels 1, 3, 6 and 19, and pyarrow
        reads the default level's frames (TestZstdPyArrow);
  - [x] the ratio at the default level is measured beside pyarrow's,
        because a faster search that loses bytes has to say how many;
  - [x] CompressBench `zstd_compress` lines, ours against aircompressor
        (16.5 vs 6.45 ms before), one lane at a time.
  - RESULT:
    - compress 16.5 -> 7.54 ms on 4 MiB of lines (a second run 7.58),
      against aircompressor's 6.45. The gap went from 2.6x to 1.17x.
    - The ratio at level 3 barely moved: `big` +2.0% (424 526 -> 433 035
      B), `numbers` -6.9% (42 639 -> 39 682), text +3 bytes, the rest
      within a few bytes.
    - TestZstdPyArrow still reads and writes against pyarrow.
    - Found on the way: level 6 (the chain, 16 deep, no lazy skip)
      compresses `big` and `numbers` WORSE than level 3 did before
      (437 853 vs 424 526; 47 927 vs 42 639). A deeper search losing
      bytes is its own defect, filed as zstd-level6-worse-than-3.
    - Left from the lead list: FSE packed decode entries and the Native
      timing, as okay-compress-zstd-speed-3.
