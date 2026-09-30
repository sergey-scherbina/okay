- [ ] freer-kont-migrate — PRIORITY: HIGH (operator, 2026-09-30:
      "Мигрируй", after the probe's numbers). The frame machine of
      specs/freer-kont.md becomes the core's one continuation runtime,
      in stages, each its own gated landing:
      1. `Frames`, `Cont0`, `Prompt`, `Frames.run` move from `okay.kont`
         into package `okay` (Cont.scala, beside `Cont`); TestKont and
         KontBenchmark follow. Nothing else changes.
      2. `Delim` over the machine: `Push`/`Dollar`/`Watched`/`Capture`
         (the four flags: take the `Reset` into `k` or stop short,
         body under a fresh reset or bare) and `Op.Shift` (a strict
         `k`: the segment run to a value, as today's `sync`) as
         clauses of `Frames.run`; the typed prompt stack `p.type *: b`
         is the `Frames` index (Materzok–Biernacki's stack of answer
         types); `Delim.Stacked`'s `Segs`/`Frames`/`Hole`/`Cut`/`Step`
         and its `loop`/`split`/`copy`/`reify` deleted; every door
         (`push`, `shift`, `control`, `shift0`, `control0`, `dollar`,
         `dollarResumed`, `abort`, `scope`, `collect`, `resumable`,
         `runNested`'s forwarding, `NoPrompt`'s diagnostics) keeps its
         spelling. Oracle: TestDelim's family, TestDollar,
         TestDollarProbe, TestStackedShift0, TestLexical, TestLayered,
         TestHandlersAsDollar, TestDelimForward; DelimBenchmark's lanes
         re-measured against the probe's rows.
      3. Decide `Cont.step`: the strict runner with its `StackSwitch`
         rooms is a different machine (a synchronous `k` that may
         recurse deep); either `Shift` becomes `Shift0` with a strict
         `k` on `Frames.run`, or it stays. Measure before deciding.
      4. Delim.scala's header ("FIRST: push is an operation, not a
         handler application ...") rewritten to what the probe found;
         docs/continuations-in-practice.md's "one machine" rule
         re-read against the machine.
      NOT in this arc: handlers with state as marks (the backlog item's
      original `Tail`/`Control`); `Freer.resume` staying the rotation is
      the standing decision (Freer self-sufficient, Cont orthogonal).
