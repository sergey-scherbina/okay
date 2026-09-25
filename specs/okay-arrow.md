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

- [ ] Stage 1: the module (cross JVM/JS/Native), the facade and the
      model; `OkayArrow` = `ArrowIpc` moved, with arrow-ipc-fast's
      optimisations (the stream written once into an array of its exact
      size, UTF-8 encoded in place with no array per string, bulk
      little-endian copies, the body read in place). The same tests on
      every platform; pyarrow and Arrow Java as oracles on the JVM.
- [ ] Stage 2: `ApacheArrow`: the facade over Arrow Java and
      `toRoot`/`fromRoot`; the refusal without Arrow on the classpath;
      both implementations read each other's streams.
- [ ] Stage 3: the measurement again, `ArrowIpcBench` with the
      facade's two implementations, B/op and time, against stage 0.
- [ ] Docs: docs/modules/okay-arrow.md — which to choose and why.
### Arrow wherever it makes sense (operator, 2026-09-25)

"все равно тогда имеет смысл использовать арроу формат максимально везде
где это имеет смысл. И тогда имеет смысл реализовать его достаточно
полноценно для того чтобы он компилировался и работал не только на jvm
а и в js и native". So `OkayArrow` grows from the wire's five columns to
okay's columnar format, on every platform:

- [ ] Stage 4: the TYPES: ints 8/16/32/64 signed and unsigned, float32
      and float64, bool, utf8/binary and their large forms, decimal128,
      date32/date64, timestamp (unit, zone), duration, fixed-size
      binary, LIST and STRUCT (nested), and dictionary-encoded columns
      decoded on read. Both implementations map every one; pyarrow and
      Arrow Java are the oracles both ways, on the JVM; the same round
      trips run on Scala.js and Native.
- [ ] Stage 5: the typed layer: `Schema[A]` rows through okay-codec's
      `Columns` (which already has Int32, Decimal, Binary, Arr, Struct):
      `encode[A](rows)` and `decode[A](bytes)`, nested case classes as
      struct and list.
- [ ] Stage 6: the IPC FILE format (magic, footer, a batch by index),
      for storage and export.
- [ ] Stage 7: where Arrow replaces CBOR — each candidate MEASURED
      against CBOR first, and moved only where it wins: batches of
      records in okay-stream / dataflow / okay-cluster; okay-persist
      export and okay-sql results; frames for R (r-arrow), TypeScript,
      Rust and Go. Control messages stay JSON/CBOR: a schema and a batch
      header per message make a short message longer (the same reason
      DEFLATE left pipes).
- [ ] Body compression (LZ4_FRAME, ZSTD): in no JDK and on no JS or
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
