# okay-arrow — the Arrow columnar format, our own

Status: stage 0 done (2026-09-25): NOT faster. The module is a FACADE with two implementations (below). Asked by the operator
2026-09-25: "можешь сделать модуль okay-arrow с нашей полноценной
реализацией формата arrow? У нас получается быстрее чем у оригинального
apache arrow?"

## Where it starts

py-arrow (specs/py-arrow.md) wrote `okay.codec.ArrowIpc`: Arrow IPC
streams for five columns (int64, float64, utf8, bool, null), read and
written by hand, pyarrow-validated. Nobody has measured it against Arrow
Java, so "faster than Apache Arrow" is a QUESTION until stage 0 answers
it. The design of everything after depends on that answer (a copy into
JVM arrays per column, as today, against Arrow's off-heap buffers read in
place).

## Stage 0 — the measurement, before any module code

JMH, one table of 500 000 rows (float64, int64, utf8 with nulls), Arrow
Java 19.0.0 as the reference, the same IPC bytes on both sides:

- WRITE from JVM arrays to IPC bytes: `ArrowIpc.write` against filling
  Arrow vectors and `ArrowStreamWriter`;
- WRITE from each library's own columns: ours are already arrays; Arrow's
  `VectorSchemaRoot` already filled (its best case);
- READ IPC bytes to JVM arrays: `ArrowIpc.read` against
  `ArrowStreamReader` plus a copy out;
- READ into each library's own columns: ours are arrays; Arrow's
  `VectorSchemaRoot` loaded, no copy out (its best case).

Interop first: each reads the other's bytes and gets the same table,
or no number is recorded. Lane rules (docs/benchmarks.md): each lane
names what it includes, and a lane where one side does work the other
skips is not compared.

- [x] Stage 0: the four pairs measured, interop checked, verdict here.

## The module: a facade, two implementations (operator, 2026-09-25)

"Делай модуль okay-arrow где будет фасад с двумя реализациями - наша
которая делает только то что нужно нам на всех платформах и
оптимизированно для нас. А вторая это просто биндинг к настоящему аппач
арроу - но зависимость к нему опциональная." — after stage 0 showed a
full reimplementation buys speed in one niche and costs months.

- `okay.arrow.ArrowCodec`: `write(Table): Array[Byte]` and
  `read(Array[Byte]): Table` over one model (`okay.arrow.Table`,
  `okay.arrow.Column`: int64, float64, utf8, bool, null; nullable; the
  schema's metadata). A given picks the implementation:
  - `OkayArrow` — OURS, the default (`given ArrowCodec` in the
    companion), on JVM, Scala.js and Native; exactly the five columns
    the wire needs, no dependency. `okay.codec.ArrowIpc` moves here.
  - `ApacheArrow` — JVM only, `import okay.arrow.ApacheArrow.given`: the
    same facade over Arrow Java 19, plus `toRoot`/`fromRoot` between the
    model and a `VectorSchemaRoot` for code that lives in Arrow Java.
    Arrow Java is an OPTIONAL dependency of okay-arrow (Maven
    `<optional>`): nobody gets it transitively; a program that imports
    `ApacheArrow` adds `arrow-vector` and `arrow-memory-unsafe` (or
    `-netty`) itself, and the JVM flags they need. Without them on the
    classpath, the first use is refused by name, saying what to add.
- okay-py depends on okay-arrow (the wire's frames), never on Arrow Java.

- [x] Stage 1: the module (cross JVM/JS/Native), the facade and the
      model; `OkayArrow` = `ArrowIpc` moved, with arrow-ipc-fast's
      optimisations (the stream written once into an array of its exact
      size, UTF-8 encoded in place with no array per string, bulk
      little-endian copies, the body read in place). The same tests on
      every platform; pyarrow and Arrow Java as oracles on the JVM.
- [x] Stage 2: `ApacheArrow`: the facade over Arrow Java and
      `toRoot`/`fromRoot`; the refusal without Arrow on the classpath;
      both implementations read each other's streams.
- [x] Stage 3: the measurement again, `ArrowIpcBench` with the
      facade's two implementations, B/op and time, against stage 0.
- [x] Docs: docs/modules/okay-arrow.md — which to choose and why.
### Arrow wherever it makes sense (operator, 2026-09-25)

"все равно тогда имеет смысл использовать арроу формат максимально везде
где это имеет смысл. И тогда имеет смысл реализовать его достаточно
полноценно для того чтобы он компилировался и работал не только на jvm
а и в js и native". So `OkayArrow` grows from the wire's five columns to
okay's columnar format, on every platform:

- [x] Stage 4: the TYPES: ints 8/16/32/64 signed and unsigned, float32
      and float64, bool, utf8/binary and their large forms, decimal128,
      date32/date64, timestamp (unit, zone), duration, fixed-size
      binary, LIST and STRUCT (nested), and dictionary-encoded columns
      decoded on read. Both implementations map every one; pyarrow and
      Arrow Java are the oracles both ways, on the JVM; the same round
      trips run on Scala.js and Native.
- [x] Stage 5: the typed layer: `Schema[A]` rows through okay-codec's
      `Columns` (which already has Int32, Decimal, Binary, Arr, Struct):
      `encode[A](rows)` and `decode[A](bytes)`, nested case classes as
      struct and list.
- [x] Stage 6: the IPC FILE format (magic, footer, a batch by index),
      for storage and export.
- [x] Stage 7a (the measurement): where Arrow replaces CBOR — each candidate MEASURED
      against CBOR first, and moved only where it wins: batches of
      records in okay-stream / dataflow / okay-cluster; okay-persist
      export and okay-sql results; frames for R (r-arrow), TypeScript,
      Rust and Go. Control messages stay JSON/CBOR: a schema and a batch
      header per message make a short message longer (the same reason
      DEFLATE left pipes).
- [x] Body compression (LZ4_FRAME, ZSTD), landed by okay-compress 2026-09-25: in no JDK and on no JS or
      Native standard library, so OURS, in a module of its own
      (`okay-compress`, usable outside Arrow too — as a
      `WireCompression` beside DEFLATE, for one), pure Scala over
      `Array[Byte]`, every platform (operator, 2026-09-25: "Мы можем
      реализовать свою компрессию ... Чтобы работала на всех таргетах?"):
      - LZ4 block + frame (xxHash32), both directions: small (a hash
        table and a copy loop); pyarrow's LZ4_FRAME bodies are the
        oracle both ways;
      - ZSTD DECOMPRESSION (RFC 8878: FSE, Huffman, sequences, repeat
        offsets): what Python's Arrow and Parquet files mostly carry;
      - ZSTD compression last, and only if LZ4's ratio is measured short.
      Until each lands, a compressed body is refused by name.

- [ ] Later, on a trigger: `OkayArrow` as VIEWS over the message's bytes
      (no copy at all on read; stage 0's Decisions entry), when a
      consumer that does not need JVM values appears; the C Data
      Interface in `ApacheArrow` (Arrow Java has it) for pyarrow in the
      same process.

## Decisions

- **Not a full reimplementation** (operator, after stage 0): ours does
  what the wire needs, on every platform; the full format is Arrow
  Java's, behind the same facade, for whoever adds it.
- **Columns as BUFFERS (views) — deferred, not refused** (stage 0's verdict).
  Arrow Java's read into its own columns is 1–1.7 ms for 500 000 rows
  because it copies buffers and makes no objects; `ArrowIpc` spends
  30–45 ms making 500 000 `String`s and boxing nothing else. The module's
  column is therefore a VIEW over the message's bytes: a buffer, an
  offset and a length per Arrow buffer, values read little-endian on
  access (`VarHandle` byte-array views, JDK 9+, so the floor 17 holds),
  and a string decoded only when asked for. Read becomes validating the
  message and slicing it — no copy at all, where Arrow Java still copies
  into off-heap memory. Write from such columns is concatenating
  buffers. `ArrowIpc`'s arrays stay as the conversion at the edge
  (`PyFrame` needs values), not the representation.
- **Heap byte arrays, not off-heap memory.** The received message is a
  heap `Array[Byte]` already; slicing it needs no allocator, no
  `--add-opens`, no reference counting, and works on Scala.js and Native
  as well. Off-heap (FFM `MemorySegment`) belongs to stage 5, the C Data
  Interface, where the memory is pyarrow's.

## Results

- Stage 0 (`ArrowIpcBench`, JMH, two rounds at load 120–150, 5+5 x 2 s,
  one fork; Arrow Java 19.0.0, unsafe allocator; both read Arrow Java's
  14.1 MB stream; interop checked both ways by `TestArrowJavaInterop`
  and again in setup). Times are noisy at that load; B/op is not:

  | 500k rows | okay round 1 / 2 | Arrow round 1 / 2 | B/op okay | B/op Arrow |
  |---|---|---|---|---|
  | write from arrays | 39.0 / 15.8 ms | 19.8 / 15.6 ms | 128.7 MB | 63.5 MB |
  | write from own columns | 122 / 62 ms (the same code as above: noise) | 15.0 / 3.2 ms | 128.7 MB | 49.8 MB |
  | read to arrays | 43.2 / 45.0 ms | 59.6 / 23.3 ms | 136.2 MB | 104.7 MB |
  | read to own columns | 42.4 / 29.8 ms | 1.7 / 1.0 ms | 136.2 MB | 14 KB |

  Level through plain arrays; 5–30x behind in each side's own columns,
  and the allocation says why: a `String` per row, and a writer that
  grows its buffer by doubling and copies it out. Hence the Decisions
  entry above.

- Stages 1–3 (2026-09-25). The module is cross (JVM, Scala.js, Native);
  `TestOkayArrow` runs on all three. `TestApacheArrow` (5): the default
  given is ours, the import picks Arrow Java, each reads the other's
  stream, `toRoot`/`fromRoot`, the refusal text. okay-py's live Arrow
  suites pass on `OkayArrow` unchanged.
  - FOUND: an sbt `% Optional` jar is on the compile classpath but not
    in a forked JMH JVM (`NoClassDefFoundError: BufferAllocator`) —
    exactly a consumer's position, so the bench and tests carry it
    explicitly (`optional;test;jmh`), and `ApacheArrow.missing` is the
    path a consumer without Arrow meets.
  - The measurement (load 230–250; B/op firm, times wide):

    | 500k rows (float64, int64, text) | `OkayArrow` before | `OkayArrow` | Arrow Java 19 |
    |---|---|---|---|
    | write from arrays | 129 MB, 16–39 ms | 17 MB, 15–18 ms | 64 MB, 15–19 ms |
    | read into arrays | 136 MB, ~44 ms | 70 MB, ~42 ms | 105 MB, 46–157 ms |
    | read into its own columns | 136 MB, 30–42 ms | 70 MB, 14–19 ms | 14 KB, ~1 ms |
    | through the facade, both ways to the model | | the rows above | write 65 MB, 14–22 ms; read 122 MB, 64–146 ms |

    Allocation on write fell 7.6x (a stream written once into an array
    of its exact size, UTF-8 encoded in place) and on read halved (the
    body read in place, bulk little-endian copies).

- Stage 4 (okay-arrow-types, 2026-09-25): the model's 16 column kinds
  (25 Arrow types with their parameters) in both implementations.
  `OkayArrow` rewritten recursively — field nodes and buffers in
  pre-order, dictionary batches (deltas too) decoded through
  `Column.take`, lists normalised to offsets from 0 (a sliced list's
  first offset is not 0 in pyarrow's stream), half floats widened by
  hand (`Float.float16ToFloat` is JDK 20+ and JVM-only). `ApacheArrow`
  maps every kind through Arrow Java's own vectors (fixed-width through
  the data buffer, lists through `startNewValue`/`endValue`, structs
  through their children), never through okay's IPC, so each checks the
  other.
  - Tests: `TestOkayArrow` (9, on JVM, Scala.js and Native: every kind
    round-trips, cut streams refused, `take`, `concat`), `TestApacheArrow`
    (7: both ways across implementations for every kind; a dictionary
    Arrow Java wrote), `TestPyArrowOracle` (3, Live: pyarrow validates
    all 25 types we write, names them, and ITS writer's stream reads back
    the same; large forms, dictionary, float16, two batches and slices
    only pyarrow makes; a map refused by name).
  - Mutant: decimal128 bytes in big-endian order — four tests red.

- Stage 5 (okay-arrow-typed, 2026-09-25): `Rows.table`/`Rows.rows` and
  `ArrowCodec.encode`/`decode` for any `Schema[A]`. okay-arrow now
  depends on okay-codec (not the other way: okay-codec stays free of
  Arrow). The write is `Columns.table` translated to the model; the read
  is a `Schema.Algebra` making Columns' decisions backwards (products by
  field NAME with codec defaults for a missing one, enums by case name,
  sums by `kind`, recursive types through their CBOR).
  - Tests: `TestRows` (6, on JVM, Scala.js and Native: a 13-field order
    with nested products, lists, options, an enum, a sum, bytes, a
    BigInt and a Char; the column types; a recursive tree; a non-product
    value; three misfits as Lefts naming row and column; no rows);
    `TestPyArrowOracle` +1 (pyarrow validates typed rows and reads the
    structs, lists and sum as ordinary Arrow).
  - Mutant: `Option` ignoring the validity — the round trip red, naming
    row 1, column 'note'.

- Stage 6 (okay-arrow-file, 2026-09-25): `writeFile`, `readFile`,
  `fileBatches` and `readFileBatch` on the facade. `OkayArrow` works on
  every platform, compressed too. `ApacheArrow` goes through Arrow Java's
  file writer and reader (uncompressed: its codecs are another optional
  jar). The layout is pyarrow's: `ARROW1` and padding, the stream WITH its
  end-of-stream marker, the footer, its length, `ARROW1`. A batch is read
  by framing the schema, the dictionaries and its block as a stream of
  their own.
  - Tests: `TestOkayArrow` +1 (every platform: plain, LZ4 and ZSTD
    files; a batch by index; a file cut at any of every fifth byte
    refused; a stream is not a file); `TestApacheArrow` +2 (files both
    ways; a failed read keeps its own reason); `TestPyArrowOracle` +1
    (pyarrow's `open_file` validates ours; ours reads batch 1 and 2 of
    pyarrow's three by index).
  - Mutant: the block offset without the 8-byte `ARROW1` header. Both
    suites went red, and the mutant FOUND A DEFECT: when Arrow Java
    failed mid-read, `ApacheArrow` closed its allocator in a `finally`,
    and the allocator's "Memory was leaked" REPLACED the reading error.
    `withAllocator` now keeps the first failure. Its test was watched red
    without the fix.

- Stage 7a (arrow-vs-cbor, 2026-09-25): `ArrowVsCborBench` sends the same
  typed rows through Arrow (Rows + OkayArrow) and CBOR (its Schema
  codec), round trip, plain and ZSTD. Three shapes: flat trades; events
  with an enum, a list and an option; nested orders. Two batch sizes:
  1 000 and 100 000 rows. Load 33–50: bytes and B/op are firm, times
  wide. pyarrow wrote the same tables for reference.

  | 1 000 rows | trades | events | orders |
  |---|---|---|---|
  | bytes: Arrow / CBOR | 28 944 / 47 443 | 55 560 / 63 411 | 85 104 / 121 738 |
  | round trip ms: Arrow / CBOR | 0.63 / 3.06 | 0.57 / 2.93 | 1.10 / 3.83 |
  | B/op: Arrow / CBOR | 1.57 / 5.43 MB | 2.24 / 6.57 MB | 4.21 / 11.2 MB |
  | bytes, ZSTD: Arrow (ours) / Arrow (pyarrow) / CBOR | 9 168 / 5 808 / 8 155 | 23 584 / 14 344 / 6 289 | 21 816 / 16 680 / 7 454 |

  At 100 000 rows the same holds for bytes and allocation (trades: 2.8 MB
  vs 4.7 MB, 145 MB vs 569 MB allocated, 61 vs 239 ms). The nested
  orders' times are wider than their gap (488 +-380 vs 335 ms).

  VERDICT, the rule for stage 7b:
  - UNCOMPRESSED (in-process, pipes), a batch of typed records is smaller,
    3.5x lighter on the heap, and 3.5–5x faster as Arrow than as CBOR:
    move batch transports that do not compress to Arrow.
  - COMPRESSED (a network), the format itself loses where there is text
    or nesting: even pyarrow's Arrow+ZSTD is 2.3x (events) and 2.2x
    (orders) larger than CBOR+ZSTD, because per-buffer compression loses
    the row-wise repetition. It wins only on flat numeric tables (trades:
    pyarrow 379 384 vs CBOR 434 398 bytes at 100k). Keep CBOR+ZSTD
    there, unless the batch is flat and numeric.
  - Our ZSTD itself was 1.3–1.6x behind pyarrow's on columnar buffers.
    FIXED by okay-compress-zstd-ratio: Arrow's offset buffers went RAW,
    because Huffman weights past 128 symbols were not written. At 1 000
    rows, Arrow+ZSTD (ours) is now:
    - trades 5 848 bytes (pyarrow 5 808, CBOR+ZSTD 7 922);
    - events 14 648 (pyarrow 14 344, CBOR+ZSTD 5 991);
    - orders 14 200 (pyarrow 16 680, CBOR+ZSTD 6 584).

    The round trip is 0.76 / 1.6 / 3.0 ms against CBOR+ZSTD's 1.7 / 2.9 /
    5.5, with less than half the allocation. So, compressed, Arrow is
    faster and lighter on every shape, and smaller only on flat numeric
    ones. CBOR+ZSTD stays 2.2–2.4x smaller where there is text or
    nesting, which is the format's own cost. Over a network the choice
    is bytes (CBOR) against CPU (Arrow).

## Stage 7b — the transports (operator, 2026-09-25: "7b")

The survey of what serializes BATCHES of typed records (grep for the
codecs over okay-*/src/main): okay-cluster's `Remote` is the one — chunks
of records between nodes, as JSON lines, "CBOR takes over when its
dialect lands" in its own header. Everything else sends one value at a
time (a partial per partition in `Wire`, a Raft image, a span, a journal
record), where Arrow has nothing to add.

- [x] `Remote` frames: a 4-byte length, a 1-byte format tag, the
      payload. The RECEIVER reads the tag, so it needs no configuration
      and one listener takes any sender. `given RemoteFormat`:
      - `Arrow` (the default: a chunk is a batch of records, and
        uncompressed Arrow wins on every shape measured);
      - `Cbor` (fewer bytes for text or nested records once compressed);
      - `Json` (the old wire, kept for comparison).

      `given RemoteCompression`: none by default, or LZ4 / ZSTD (Arrow:
      per buffer, in the IPC body; CBOR and JSON: the whole payload in a
      frame). The listener's contract is unchanged: a damaged frame is
      dropped and the stream lives, the wire closing closes the channel
      after the buffered chunks.
- [x] The number: the same chunks through a real socket in each format,
      bytes on the wire and time end to end.

- Stage 7b (remote-arrow-frames, 2026-09-25): `Remote` sends tagged
  binary frames, `RemoteFormat` (Arrow by default, CBOR, JSON) and
  `RemoteCompression` (none, LZ4, ZSTD). The listener reads the tags.
  `TestRemote` (8, Live: every format and compression to a listener told
  nothing; a damaged frame, a stranger's payload and an unknown tag all
  dropped with the stream living). `MeasureRemote` (Live, medians of
  five, 200 000 trade records, load 24–31):

  | chunk | JSON | CBOR | Arrow | CBOR+ZSTD | Arrow+ZSTD | Arrow+LZ4 |
  |---|---|---|---|---|---|---|
  | 1 000 | 136 ms, 13.7 MB | 182 ms, 9.8 MB | 59 ms, 7.1 MB | 214 ms, 1.46 MB | 159 ms, 1.13 MB | 76 ms, 3.1 MB |
  | 10 000 | 137 ms | 188 ms | 50 ms, 6.9 MB | 216 ms, 1.49 MB | 141 ms, 0.95 MB | 58 ms, 2.9 MB |

  - FOUND and FIXED: CBOR's and JSON's `List` decoders were QUADRATIC
    (`xs :+ _` on a List, one copy per element). Before the fix, JSON took
    13.3 s and CBOR 7.0 s for chunks of 10 000. `TestListDecodeLinear`
    (okay-codec) was watched red at 463x a Vector's read, and green after
    (prepend and reverse, as Edn already did).
  - FOUND: "localhost" can reach a stranger on this box, over IPv6 on
    the same ephemeral port number. That gave a Broken pipe once and a
    stall once. The in-JVM thread dump showed our listener still in
    `accept` after the sender had finished; the gate's own dump is of
    sbt, not of the forked JVM. The tests now bind and connect to the
    loopback address. The same shape in three other suites is filed as
    `localhost-connects-to-a-stranger`.


## Stage 8 — dictionary-encoded columns, kept and written (arrow-dictionary, 2026-09-26)

**Why.** okay-watch's production scoring (its specs/scoring.md, the
operator's addendum 2) sends chunks to R and Python as Arrow record
batches and asks that factors survive the round trip EXACTLY — the
levels, their order, the unused ones — dictionary-encoded on the wire.
R's `arrow` maps a dictionary to a factor and back by itself; pyarrow to
a pandas `category`. What cannot carry it is the JVM side: the model has
no dictionary column, the reader decodes one to its values (the levels
and their order are lost), and the writer has nothing to write.

**The model.** `Column.Dictionary(indices: Array[Int], dictionary:
Column, ordered: Boolean, valid: Array[Boolean])` — int32 indices, the
index type every producer here meets (R, pyarrow, Arrow Java all default
to it), into a dictionary column of any kind; `decoded` is the column the
reader answered before this stage (`dictionary.take(indices, valid)`).

**Reading.** `OkayArrow.read` is UNCHANGED — dictionaries decoded, as
every consumer written so far expects. `OkayArrow.readKeeping(bytes)`
answers a TOP-LEVEL dictionary-encoded field as a `Dictionary` (a nested
one is still decoded; nothing asks for it). Batches whose dictionaries
are equal concatenate their indices; different ones (a replacement
dictionary between batches) are appended and the later indices shifted,
so every row still reads its own value.

**Writing.** A `Dictionary` column is written as Arrow writes one: the
field declares `DictionaryEncoding{id, int32 signed, isOrdered}` with the
VALUE type as its type, a `DictionaryBatch` for it precedes the record
batch, and the record batch holds the indices. Top-level only; a
dictionary nested in a list or struct is refused by name. The IPC FILE
writer lists the dictionary blocks in its footer.

**The rest of okay.** Every match on `Column` that has no use for the
encoding treats a `Dictionary` as its `decoded` column (the frames of
okay-py and okay-r, Rows, the lake, parquet, the C Data bridge), so no
consumer sees a new kind. `ForeignWorker.frameTable(…, keepDictionaries
= true)` reads its answer with `readKeeping`; the default stays the
decoded one. `ApacheArrow` writes a `Dictionary` decoded (its values)
and reads as before — stated, not hidden: the dictionary road is
OkayArrow's.

### Behavior

- [x] a `Dictionary` column round-trips through OkayArrow's stream and
      file writers and `readKeeping`: indices, the dictionary's values
      and order (unused values included), `ordered`, nulls —
      TestArrowDictionary; the file read decoded (`readFile` keeps
      nothing — no caller asks), pyarrow opens it from the footer
- [x] `read` of the same bytes answers the decoded column, as before —
      TestArrowDictionary; TestOkayArrow unchanged and green
- [x] a stream from pyarrow with a dictionary column reads kept, and
      ours is pyarrow's `dictionary<values=string, indices=int32,
      ordered=1>`, levels in order — TestPyArrowOracle (Live; pyarrow
      19.0.1). R's side is exercised by okay-watch's scoring tests over
      R + arrow in a container.
- [x] two batches with one dictionary concatenate; a replacement
      dictionary shifts the later indices and every row reads its value —
      TestArrowDictionary (on `Column.concat`, what the reader calls)
- [x] a dictionary nested in a struct is refused on write, by name; an
      index outside its dictionary too — TestArrowDictionary
- [x] okay-py's and okay-r's frames read a `Dictionary` as its values —
      TestArrowFramesDictionary, TestRArrowFramesDictionary; `Rows` too
