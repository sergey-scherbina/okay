- [ ] cont-run-prompt — operator ask (2026-10-03): Cont's one static root
      prompt replaced by a fresh `Prompt` per run (`Cont.run`/`reset`), nested
      `shift`s capturing to the nearest one. A leaf is data built before any
      run, so it names the root's KIND: `Prompt.kind` (the prompt itself for
      every other prompt, so their matching is unchanged), and the machine's
      capture matches `sh.p eq d.p.kind`. API unchanged; the answer-type claim
      stays (one delimiter, many leaf answer types — cont-typed-claim).
      Measure statePara / contAnswer / fib lanes against master: one field
      read on the capture path, one Prompt per run.
