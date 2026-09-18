- [ ] collect-early-stop — `Delim.collect` has no way to stop: `exit`
      inside the body does not resolve (collect hands out `Emitting`,
      not `Prompted`), and aborting the collect's own prompt would
      drop the cons frames of everything already emitted, so the
      prefix cannot be answered with. The shape that wants it is
      `take n` over a push producer; `Generate`/`Producer` already
      covers it by making the producer lazy, and the practice doc now
      points there. Revisit only if a consumer wants the eager form
      with a stop (continuations-audit, 2026-09-17).
