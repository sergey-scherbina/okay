- [x] handling-ever-per-machine — ANSWERED 2026-10-03 by cont-atm (see the end). Was: PRIORITY: LOW, MEASURED 2026-10-03
      (the lane of this name; DelimBenchmark.stateForeign/
      stateForeignEver, src/jmh/history.d handling-ever-flag).
      `Cont0.Handling.ever` (Delimited.scala) is a PROCESS-WIDE flag: once
      any handler frame (handle-frames) has been built anywhere in the
      JVM, every machine looks down its stack (`handlerFor`) before
      forwarding an operation of another effect. What that look costs:
      N State operations on the machine under ONE delimiter, answered
      outside, 33.7 us with the flag off against 35.6 us forced on —
      1.055x, a deeper stack more. So the flag is worth keeping, and
      deleting it (the simplification hoped for) is REFUTED. What stays
      open is the design the item named: the machine pushes and pops
      every `Dollar` itself, so it could know whether ITS stack holds a
      `Handling` frame (a bit on the `Dollar` node, or a count) instead of
      a whole process paying after one handler anywhere — a lane in a
      test fork that ran a handler earlier measures 5% slower for that.
      Weigh against C2's register pressure in this loop
      (cont-frames-register-pressure) before adding a register.
      Since cont-run-prompt (2026-10-03) a frame whose operations mark
      themselves `Cont0.Framed` is looked up WITHOUT the flag
      (`Handling(name, opens = false)`): Cont's run frames leave it off.
      The flag still turns on with the first handle-frames frame.
      ANSWERED by cont-atm (2026-10-03, specs/cont-atm.md §5): the λ$
      machine and both process-wide flags are gone. On `Delimited` a
      run's own steps (Shift's `Steps`) set `framed` when a `Handling`
      frame is installed IN THAT RUN — only then does an operation of
      another effect look at the boundaries (`holds`, which walks
      boundaries, never frames) — and a catch frame turns on `guarding`
      for that run only. A lane in a fork that ran a handler earlier no
      longer pays for it.
