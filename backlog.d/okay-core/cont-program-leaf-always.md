- [ ] cont-program-leaf-always — PRIORITY: HIGH (operator, 2026-10-04: the
      host stack only where nothing else can work; specs/cont-stack.md, "THE
      HOST STACK ONLY WHERE NOTHING ELSE CAN WORK", road 2). An opaque body
      whose answer `S` is a program (`Cont.Later[S]` exists) gets the program
      leaf whenever it MENTIONS `k`. Today it gets it only when it CALLS `k`
      directly (ContMacro `opaque`, `calls` skips lambdas). Then
      `runChoice`, `runSeq` and `runExact` (`k` inside a `foldM` step) and
      the Scala 2 facade's `Effect.handle` stop nesting a strict run per
      operation on the host stack. The contract change, already written for
      the program leaf: host side effects after `k(a)` in the body run
      before `k`'s rest. Inside a lambda nothing promised otherwise.
      RED FIRST: a `Choose` program 100 000 choice points deep on a small
      thread, with `-Dokay.cont.room` high, overflows today and must not.
      MEASURE: a Choice/Prob lane, before against after.
