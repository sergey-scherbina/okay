## handler-single-pass-dispatch: the fused loop dispatches by a table, not a chain of tests

- specs/handler-single-pass.md, "Dispatch" (operator, 2026-10-04). At run time the dispatch is a per-stack
  table keyed by the operation's exact class: a lazy cache over the `TypeableK` tests, where a hit costs one
  class compare and a direct call of the step. At compile time, when one `handle(h1, h2, h3)` names the
  whole stack, a macro writes the loop with the steps inlined. The limits are written down: instances by
  name, the megamorphic step call, and a short chain that C2 may already make fast. Stage 0 measures chain
  against table.
