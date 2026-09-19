- [ ] handler-fusion-step — GATED OFF by stage 0, same reason. Was:
      `Step[F, Acc]` (tail-resumptive by type)
      and `Fused.run` over `F + G` with the row-shaped product state;
      instances for State, Writer (Fold-generic), Reader incl. local;
      abort/choose fall back to a shift with the state captured
      immutably; laws: agrees with nested for both orders, stack-safe
      at 1M, multi-shot and abort survive.
      (was filed under "handler-fusion" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
