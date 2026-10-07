- [ ] cont-classic-rename — LAST, after effects-rows and
      cont-first-module: the names. `okay.freer.Cont` is the CPS paramonad
      today (classic-to-freer landed it there, 2026-10-07; `okay.freer.cps`
      chooses it as Effects' carrier); the machine is `okay.cont.Cont`. When the machine is the core's program, the
      bare name goes to the machine and the CPS one under its module's
      name; `/>` and `!>` follow (85 and 13 files use them). A rename, not
      logic: done last so nothing is renamed twice. Also record: the
      Shift effect, ShiftMachine and HandleFrames stay with the classic —
      dynamic prompts have no place on the machine by design (stage 12;
      the level-2 finding of 2026-10-06).
