- handler-fusion-step — CLOSED 2026-09-23 with the handler-fusion arc: `Fused` is a
  TEST fixture since fused-out-of-core (2026-09-22) and no production file
  stacks two continuation-aware handlers, so a `Step`/`Fused.run` product
  state has no consumer to serve. Moved here by backlog-audit-0923. Was:
  GATED OFF by stage 0, same reason; and before that:
      `Step[F, Acc]` (tail-resumptive by type)
      and `Fused.run` over `F + G` with the row-shaped product state;
      instances for State, Writer (Fold-generic), Reader incl. local;
      abort/choose fall back to a shift with the state captured
      immutably; laws: agrees with nested for both orders, stack-safe
      at 1M, multi-shot and abort survive.
      (was filed under "handler-fusion" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
