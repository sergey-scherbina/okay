# okay-compress

LZ4 and ZSTD of our own (specs/okay-compress.md): pure Scala over byte
arrays, on the JVM, Scala.js and Scala Native, with no dependency. The
JDK has neither codec. The JVM's usual choices are native bindings
(lz4-java, zstd-jni) or a pure-Java port that uses `Unsafe`
(aircompressor), and none of them runs on JS or Native.

| | |
|---|---|
| `Lz4Frame` | the LZ4 frame format, both ways: independent blocks of up to 4 MiB, the content size, the XXH32 content checksum; any frame read (dependent blocks, block checksums, skippable frames) |
| `Lz4Block` | a bare LZ4 block, into a buffer you give it (the format Arrow's `lz4_raw` and aircompressor use) |
| `Zstd` | ZSTD frames (RFC 8878), both ways |
| `XxHash` | XXH32 and XXH64, the two frames' checksums |
| `Codec` | the facade: `name`, `compress`, `decompress` |

## Using it

```scala
      val c = Lz4Frame.compress(b)
      assertEquals(Lz4Frame.decompress(c).toVector, b.toVector, name)
```

```scala
      assertEquals(Zstd.decompress(Zstd.compress(b)).toVector, b.toVector, name)
```

Input a codec cannot read is refused by name with `Corrupt`, never
answered with wrong bytes. That covers a frame cut short, a checksum that
does not match, and a feature not implemented (a dictionary).

## What the ZSTD side does

- **Decompression** reads every frame RFC 8878 defines without a
  dictionary:
  - raw, RLE and compressed blocks;
  - literals raw, RLE, or Huffman-coded in one or four streams, with a
    table carried or reused;
  - sequences FSE-coded with predefined, RLE, carried or repeated tables;
  - the three repeat offsets and the XXH64 checksum;
  - concatenated and skippable frames.

  pyarrow's frames at levels 1, 3, 9 and 19 decode on every platform
  (fixtures of pyarrow's output run on Scala.js and Native too).
- **Compression** writes single-segment frames:
  - LZ77 with hash chains, one step of lazy matching and the repeat
    offsets tried first;
  - sequences FSE-coded with the predefined tables, or with a table of the
    block's own when that is shorter;
  - literals Huffman-coded when that is shorter than storing them raw.

  The FSE encoder is DERIVED from the decoder's own table (for each symbol,
  which state leads to each next state), so the two cannot disagree.

## Arrow

okay-arrow uses these for Arrow's compressed IPC bodies: `LZ4_FRAME` and
`ZSTD`, per buffer, both ways, and pyarrow reads what we write:

```scala
      assertEquals(Tables.same(Tables.everything, OkayArrow.read(OkayArrow.write(Tables.everything, Some(codec)))), None, codec.name)
```

## Measured

`CompressBench` (JMH) against aircompressor, a pure-Java port. The box was
loaded (35–112), so the times are wide; the pairs ran side by side:

| 4 MiB, JMH, `lines` / `text` | okay-compress | aircompressor 2.0.3 |
|---|---|---|
| LZ4 compress (block) | 4.9 / 5.2 ms | 5.7 / 3.6 ms |
| LZ4 decompress (block) | 2.7 / 3.6 ms | 1.3 / 0.9 ms |
| ZSTD compress (level 3) | 30 / 15 ms | 6.6 / 2.1 ms |
| ZSTD decompress | 7.1 / 5.2 ms | 3.6 / 0.8 ms |
| LZ4 output | 1 021 339 / 1 746 074 bytes | 1 029 432 / 1 929 484 bytes |
| ZSTD output | 161 076 / 167 633 bytes | 236 268 / 170 999 bytes |

The output is at least as small as the reference's, and ZSTD's is
smaller: the hash chain searches more than aircompressor's level 3 does.
Speed is behind by 2–7x. aircompressor reads and copies eight bytes at a
time through `Unsafe`, and this code is one source for three platforms,
byte by byte. A JVM-only fast path is filed in the backlog as
`okay-compress-jvm-fast-paths`.

## Literature

- Yann Collet, Murray Kucherawy. *[Zstandard Compression and the 'application/zstd' Media Type](https://www.rfc-editor.org/rfc/rfc8878)*. RFC 8878, 2021. The format decoded and written here.
- Yann Collet. *[LZ4 block format](https://github.com/lz4/lz4/blob/dev/doc/lz4_Block_format.md)* and *[LZ4 frame format](https://github.com/lz4/lz4/blob/dev/doc/lz4_Frame_format.md)*.
- Jarek Duda. *[Asymmetric numeral systems: entropy coding combining speed of Huffman coding with compression rate of arithmetic coding](https://arxiv.org/abs/1311.2540)*. 2013. The tANS that ZSTD's FSE is.
- Jacob Ziv, Abraham Lempel. *[A universal algorithm for sequential data compression](https://doi.org/10.1109/TIT.1977.1055714)*. IEEE Trans. Inf. Theory 23(3), 1977. LZ77, the matching both codecs do.
- David A. Huffman. *[A method for the construction of minimum-redundancy codes](https://doi.org/10.1109/JRPROC.1952.273898)*. Proc. IRE 40(9), 1952. ZSTD's literals.
