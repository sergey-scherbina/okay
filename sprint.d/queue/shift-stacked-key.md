- [ ] shift-stacked-key — specs/shift-merge.md stage 3: `Shift.Stacked`
      read as `Shift % p.type`, the type-level prompt stack through the same
      row as the keyed and dynamic forms; and the keyed `reset`'s
      `ThreadLocal` room (`ResetRoom`, against cont-stack's rule) replaced
      so a nested `reset` never nests the host stack — measured on a 128 KB
      thread and on Scala.js. (2026-10-02, after shift-merge-guard)
