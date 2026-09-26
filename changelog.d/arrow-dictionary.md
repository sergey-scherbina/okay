## arrow-dictionary - dictionary-encoded columns kept and written (okay-arrow stage 8)

Asked by okay-watch's production scoring: chunks cross to R and Python as
Arrow, and an R factor must come back EXACTLY — its levels, their order,
the unused ones — which the JVM side could not hold: the model had no
dictionary column, the reader decoded one to its values, the writer had
nothing to write.

- okay-arrow: `Column.Dictionary(indices: Array[Int], dictionary: Column,
  ordered, valid)` and `decoded`; OkayArrow writes it as Arrow does (the
  field's `DictionaryEncoding` with int32 indices, a `DictionaryBatch`
  ahead of the record batch, the file footer's dictionary blocks) and
  `OkayArrow.readKeeping` answers a top-level dictionary field kept.
  `read` is unchanged (decoded). `Column.concat` joins parts with one
  dictionary by their indices and a replacement dictionary by shifting.
  A nested dictionary is refused on write by name. ApacheArrow writes a
  `Dictionary` decoded — the dictionary road is OkayArrow's.
- okay-py / okay-r: the frames and `Rows` read a `Dictionary` as its
  values; `ForeignWorker.frameTable(…, keepDictionaries = true)` answers
  them kept (default unchanged).
- Tests: TestArrowDictionary (4), TestArrowFramesDictionary,
  TestRArrowFramesDictionary; TestPyArrowOracle +1 (Live, pyarrow 19.0.1:
  ours is pyarrow's `dictionary<values=string, indices=int32, ordered=1>`
  in stream and file, pyarrow's reads kept). specs/okay-arrow.md stage 8.
