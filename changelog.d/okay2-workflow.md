## okay2-workflow - Wf and Proc in okay2

okay-workflow as `okay2-workflow`.
- `Wf`: the runtime's own questions, journalled beside the author's; the
  `Asks` doors; `Runtime`; `resumable`/`drive`/`advance`/`replay`/
  `replaying`; patching by journal tag; timers, signals, child runs and
  cancellation.
- `Proc`: the free arrow over a row, with `foldMap`, `leaves`, `render`,
  `mermaid`, `Par`, `Undo` and `Iter`.
- `Wf.Proc`: a term run on the engine; `walk` agrees with `replay` on
  every prefix; `compensating`, `accepts`, `strands`.

Each interpreter is a method on the node, so there is no cast
(specs/okay2.md stage 27). `ProcMacro` (`Proc.direct`) is not ported.
40 tests.

Docs: docs/okay2.md section 30.
