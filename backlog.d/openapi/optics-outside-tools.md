- [x] optics-outside-tools — DONE (2026-09-10, 21bc8a5c), and it was not
      on this list. Stage 2 of the spec: a tool was declared three
      times (hand-written JSON Schema, a dispatch map re-reading the
      same field names as string literals, and the name written
      twice). `Toolbox` gives `specs` and `table` from one vector.
      The finding: the hand-written schemas never declared `required`,
      so no model was ever told `board_add` needs both its fields.
