- [ ] shift-capture-objects — PRIORITY: LOW (2026-10-04). A Shift capture
      allocates `Found` and `Cut`; a resumption is `Resumption` → `Return` →
      `Resume` → `Inject` → `Nested` → `Delay`. `Resume` as its own `Pending`
      (no `Nested`) was REFUTED (delimited-cleanups: fewer bytes, slower);
      what is left to try is the capture side.
