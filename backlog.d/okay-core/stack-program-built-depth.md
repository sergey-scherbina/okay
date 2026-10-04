- [ ] stack-program-built-depth — PRIORITY: MEDIUM (stack-host-three audit,
      2026-10-04; specs/stack-safety.md, "Audit under the host-stack
      rule"). 59 recursions in the inventory are bounded only by the depth
      of a VALUE the program builds at run time, which a loop can make as
      deep as it likes. The operator's rule wants the host stack only
      where its bound is known. Move them to a heap work-list or a
      trampoline, a module per lane, high first: okay-workflow (Proc, 5),
      okay-stream (Plan/pipeline, 7), okay-lex (combinator chains, 5),
      okay-actor (hierarchy, 2), okay-cluster (Flow, 1). Then okay-ui and
      ui-gtk (23), then the low rows (stm orElse, pg/r2dbc SqlValue,
      refine patterns, script page tree). RED FIRST each time: build the
      structure 100 000 deep in a loop, on a small thread.
