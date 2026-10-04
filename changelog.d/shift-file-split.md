## shift-file-split — Shift's machine steps in their own file

From the backlog of what is still too complex (2026-10-04).

- `ShiftMachine.scala`: `Steps` (prompts, handler frames, throws on
  `Delimited`), `Nested` and `Pending`, the barrier, `promptOf` and `claim` —
  the one place Shift's claims about a `Free` row are made.
- `Shift.scala` keeps the API and the operations themselves (`Push`,
  `Dollar`, `Shift0`, `Abort`, `Resume` extend the sealed `Shift`, so they stay
  in its file) and `Resumption`; it imports the machine's, and `Shift.Pending`
  is exported. 871 lines become 659 + 227; no behaviour changes.
