- [ ] handling-ever-per-machine — PRIORITY: LOW, design and measure
      first (core-simplify review, 2026-10-02). `Cont0.Handling.ever`
      (Delimited.scala) is a PROCESS-WIDE `@volatile` flag: once any
      handler frame (handle-frames) has been constructed anywhere in
      the JVM, every machine looks down its stack for a handler
      (`handlerFor`) before forwarding a foreign operation, and reads
      the volatile on that path. So a lane's speed depends on whether
      some unrelated code — another test in the same fork — ever built
      a handler, and the "nothing pays until handlers exist" intent
      holds only per process. The machine pushes and pops every
      `Dollar` itself, so it can know whether ITS stack holds a
      `Handling` frame (a count in the loop's registers, or a bit on
      the `Dollar` node set at push). Measure against the flag on
      writerTell/layered (foreign ops, no handlers) and on a
      handle-frames lane (handlers present) before choosing; C2's
      register pressure in this loop is already a filed diagnosis
      (cont-frames-register-pressure), so a fourth register may cost.
