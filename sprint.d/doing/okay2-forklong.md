- [ ] okay2-forklong — port `Scheduler.forkLong` (okay, 2026-09-29,
      adaptive-chunked-merge-cost): a fork the caller declares long, `fork`
      by default, and on `own`/`adaptive` a sleeping worker claimed
      (`Worker.waking`) and woken at once; the chunked channel feeds fork
      with it. okay2's default is still Loom, so nothing is slow there
      today — this is parity, for a program that picks `adaptive`.
      specs/adaptive-chunked-merge-cost.md. (2026-09-29)
