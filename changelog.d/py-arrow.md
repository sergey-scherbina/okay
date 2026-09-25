## py-arrow - frames cross to Python as Arrow, 5.5x on 500k rows

Measured first (the backlog item's own condition): the Python side of a
500k-row frame was 909 ms on the JSON road and 0.5 ms as Arrow IPC.

- `okay.codec.ArrowIpc`: Arrow IPC streams written and read by hand for
  int64, float64, utf8, bool and null, all nullable, with the schema's
  metadata. No Arrow Java. pyarrow validates what it writes; it reads
  pyarrow's streams, several batches included; a stream cut at any byte
  is refused by name.
- The wire: a frame request is ONE Arrow stream with the request header
  in its metadata (`okay`), and so is the answer. It is told apart from
  JSON/CBOR by its `FF FF FF FF` marker. The shim announces
  `frames: ["arrow"]` when pyarrow is installed and imports it only
  when needed. `given FrameFormat`: Arrow where spoken by default,
  `FrameFormat.Json` never, `FrameFormat.Arrow` strict. `ForeignWorker.wire`
  says `+arrow`, and `arrowFrames` counts which road frames took.
- Python functions are unchanged (a dict of lists in, a dict, pandas or
  Table out); `@okay.arrow` hands them the `pyarrow.Table`. Answers are
  normalised to the five columns; the rest answers as JSON.
- Measured (`MeasurePyArrow`): 500k rows 806 -> 147 ms, 89 ms with
  `@okay.arrow`; 100k rows 167 -> 43 / 27 ms.
- Tests: `TestArrowIpc`, `TestArrowPy`, `TestArrowFrames`. Mutant: the
  shim never answering Arrow, caught by the counters.
- Docs: python-and-r.md "Frames as Arrow", with Raasveldt & Mühleisen
  (PVLDB 2017) and Abadi et al. (SIGMOD 2008). Spec: specs/py-arrow.md.
