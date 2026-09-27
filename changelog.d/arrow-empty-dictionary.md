## arrow-empty-dictionary - a zero-row table keeps its dictionary column

- pyarrow's `write_table` and R arrow's `write_ipc_stream` send a
  zero-row table as its schema only, so a dictionary column's levels
  were lost. `OkayArrow.readKeeping` then returned such a field as an
  empty Utf8. okay-watch's round-trip property found it at seed 83.
- Both shims now write a zero-row answer as one empty record batch,
  which carries the dictionary:
  - okay-py uses `write_batch`;
  - okay-r uses `RecordBatchStreamWriter` over `record_batch(df)`.
- The reader keeps the field's type:
  - kept, it is a `Column.Dictionary` over the dictionary that came, or
    over an empty one;
  - decoded, it is the value type rather than Utf8 always.
- Tests, each red first:
  - `TestArrowDictionary` (default gate) reads a stream that is only a
    schema;
  - `TestFrameTableExact` (Live: straight, and through pandas) and
    `TestRArrowZeroRows` get the levels back in order.

Spec: specs/okay-arrow.md "arrow-empty-dictionary".
