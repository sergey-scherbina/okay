- [ ] arrow-empty-dictionary — a ZERO-ROW table with a dictionary
      column loses it on the way back from Python or R: pyarrow's
      `write_table` and R arrow's `write_ipc_stream` write the schema
      alone (no dictionary batch, no record batch), and
      `OkayArrow.readKeeping` answers a dictionary field with no
      dictionary as an empty Utf8. okay-watch's round-trip property,
      seed 83. Fix: both shims write their answer as ONE record batch
      (an empty one included, which carries the dictionary), and the
      reader keeps a dictionary field whose dictionary never came as a
      `Column.Dictionary` over an empty dictionary.
