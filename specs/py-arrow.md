# py-arrow — frames cross to Python as Arrow

Status: all four stages landed (2026-09-25). Backlog item `py-arrow` (okay-py), promoted
2026-09-25 by the operator ("Хорошо делай py-arrow") after its own
condition was met: measure again before building.

## Why: the measurement (2026-09-25)

The same frame (three columns: float64, int64, string) on the Python
side of today's road against Arrow IPC, pyarrow 25.0.1, CPython 3.13, a
loaded box (load ~50). Python side only; our JVM side is separate:

| rows | JSON bytes | Arrow bytes | today: `json.loads` + `dec` + `enc_frame` + `json.dumps` | Arrow read + write | Arrow read + `to_pydict` |
|---|---|---|---|---|---|
| 100 000 | 2.8 MB | 2.8 MB | 16 + 26 + 25 + 37 = 104 ms | 0.3 ms | 26 ms |
| 500 000 | 15.2 MB | 14.4 MB | 189 + 285 + 240 + 196 = 909 ms | 0.5 ms | 62 ms |

The Python half of a frame's round trip is element-by-element Python;
Arrow's is a buffer handed over. The bytes are the same: the win is CPU,
on the side that has least of it. And a function that WANTS a
`pyarrow.Table` (or pandas, one call away) gets one instead of a dict of
lists it must convert itself.

## The design

- **No Arrow Java.** The JVM side writes and reads Arrow IPC itself
  (`okay.codec.ArrowIpc`), for the columns a `PyFrame` carries: int64,
  float64, utf8, bool, and the null type; every one nullable. Arrow
  Java would bring netty/unsafe memory and `--add-opens` for four
  column types; okay already writes its own CBOR for the same reason.
  pyarrow is the test oracle for every byte.
- **One message, one Arrow stream.** A frame request is an Arrow IPC
  STREAM (schema, one record batch, end-of-stream) whose schema
  `custom_metadata` carries the request header under the key `okay`:
  `{"id":7,"op":"frame","fn":"m:f","args":[..]}`. The answer is the
  same shape with `{"id":7,"ok":{"t":"arrow"}}`. A condition answers as
  an ordinary message. An Arrow stream begins with the continuation
  marker `FF FF FF FF`, which begins no JSON text and no CBOR item of
  this wire, so a reader tells the two apart by the first four bytes
  (after the per-message compression, which applies unchanged).
- **Negotiated like everything else.** The shim announces
  `"speaks":{..,"frames":["arrow"]}` when `pyarrow` is importable
  (`importlib.util.find_spec`, so a worker without it starts as fast
  as before, and pyarrow is imported on the first Arrow frame).
  `given FrameFormat` in okay.codec:
  - the DEFAULT is a preference: Arrow where the far side speaks it,
    else the JSON frame, with no refusal;
  - `FrameFormat.Json.given` never uses Arrow;
  - `FrameFormat.Arrow.given` is strict: refused by name where not
    spoken.
  Arrow needs frames on the wire, so a `configure` carrying
  `"frames":"arrow"` switches the link to length-prefixed messages even
  for `json/none`.
- **Per frame, a frame that Arrow cannot carry takes the JSON road.**
  A column mixing types, or holding a big int, bytes, a list, a dict or
  a handle, is not one of the five Arrow columns. The preference then
  sends that frame as JSON, as it does for a far side that lacks Arrow.
  The strict given refuses it by name.
- **The Python function's contract does not change.** It still receives
  a dict of lists (`Table.to_pydict()`) and may answer a dict, a pandas
  frame or a `pyarrow.Table`. A function decorated `@okay.arrow`
  receives the `pyarrow.Table` itself. On the way back the shim
  normalises the table: every int type to int64, every float type to
  float64, large and view strings to utf8, dictionaries decoded. A table
  still outside the five columns answers as a JSON frame.

## Behaviour

- [x] Stage 1: `okay.codec.ArrowIpc` writes a stream pyarrow reads
      (every column type, nulls, NaN distinct from null, empty frame,
      zero-column frame, non-ASCII text) and reads the streams pyarrow
      writes (the same, plus several record batches and absent
      validity buffers). A stream cut short, at any byte, is refused.
      The metadata header round-trips.
- [x] Stage 2: the shim announces `frames: ["arrow"]` when pyarrow is
      present, and serves an Arrow frame request with an Arrow answer:
      `to_pydict` by default, the `pyarrow.Table` under `@okay.arrow`,
      normalised on the way out, JSON when it cannot be.
- [x] Stage 3: `ForeignWorker` sends a frame as Arrow when negotiated
      and expressible, and reads either answer. The `FrameFormat`
      givens: the default preference, `Json`, and the strict `Arrow`.
      `ForeignWorker.wire` names it (`json/none+arrow`).
- [x] Stage 4: the end-to-end number. A frame of 100k and 500k rows
      through a real worker, JSON against Arrow, with our JVM side split
      out. Docs, with the table.

## Decisions

- **Header in the schema's metadata rather than a second message.**
  Two messages per request would need the link to carry pairs, and
  every link (pipes, TCP, the gateway) to agree. One Arrow stream is one
  message on any link as it is.
- **Normalise on the Python side.** pyarrow casts in C; the JVM reader
  then needs five column types, not thirty.

## Results

- Stage 1: `okay.codec.ArrowIpc`, FlatBuffers written front to back
  (every uoffset forward, vtables before their tables, struct vectors
  8-aligned). `TestArrowIpc` (5, no Python: round trip, empty, no
  columns, a cut at EVERY byte refused, JSON/CBOR never mistaken for a
  stream) and `TestArrowPy` (5, pyarrow 25.0.1: `validate(full=True)`
  on what this writes; pyarrow's streams read, three batches included,
  an all-valid column with no validity buffer; int32 refused with "cast
  it to int64").
- Stages 2–3: the shim announces `frames: ["arrow"]` by
  `importlib.util.find_spec` and imports pyarrow on the first Arrow
  frame. `TestArrowFrames` (10): the default is `json/none+arrow`; a
  round trip of every column kind with None in each; the function still
  gets Python's own types; `@okay.arrow` gets a `Table`; int32/float32
  answers normalised; a list answer and a mixed-kind request take the
  JSON road (the `arrowFrames` counters prove which road); the strict
  given refuses the mixed column by name and a worker without pyarrow at
  open; `FrameFormat.Json` stays plain; CBOR + DEFLATE compose.
  - Mutant: the shim never answering Arrow. Three tests red — the round
    trip, the normalisation and the CBOR case — through the counters;
    the values alone could not see it.
- Stage 4 (`MeasurePyArrow`, load ~200 by the OS's count, yet the JSON
  road read 806 ms against 940 ms measured quiet before, so the box was
  not starving this run):

  | rows | JSON rt | Arrow rt | `@okay.arrow` rt | JSON enc / dec (JVM) | Arrow enc / dec (JVM) |
  |---|---|---|---|---|---|
  | 100 000 | 167 ms | 43 ms | 27 ms | 14 / 21 ms | 9 / 5 ms |
  | 500 000 | 806 ms | 147 ms | 89 ms | 65 / 104 ms | 41 / 23 ms |

  5.5x by default, 9x when the function takes the table. The bytes are
  the same (13.7 MB JSON, 14.4 MB Arrow): the cost was never the size.
