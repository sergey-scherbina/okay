- [ ] delim-forwarding-default — PRIORITY: LOW, CONDITIONAL: only if
      freer-kont-frames-probe says the frame runtime is too expensive,
      and only when a real program needs `delimited` inside a Delim row
      (nothing has asked; `scope`/`collecting`/`pausing` cover every case
      seen). THE CHANGE: keep the machine, make every running combinator
      (`delimited`, `collect`, `resumable`, `Stacked.delimited`) forward
      a capture whose prompt it does not hold — `runNested`'s behaviour,
      chosen by the row (`Delim` in F: forward; not: root, `NoPrompt`) —
      so `OneMachine` and "the second rule: one machine"
      (docs/continuations-in-practice.md) go, and handlers may sit in any
      order between delimiters. THE COST, which is why it is not done
      "in any case": `OneMachine` is a SAFETY guard today. A recursion
      that installs a delimiter per level through `scope` runs on one
      machine in constant stack; the same recursion through `delimited`
      is a compile error. After this change it compiles, every level is
      a nested machine (a JVM frame chain, the bound
      freer-kont-frames-probe records for all handler loops), and it
      dies with StackOverflowError at depth — a compile error traded for
      a run-time failure with no written bound. The lane must keep a
      guard for that (a nesting counter per thread, refused by name, or
      keeping `OneMachine` for recursion-shaped call sites) or not land.
      WHAT SURVIVES EITHER WAY, and should be written first: the tests of
      the semantics — a capture from an inner delimiter to an outer one
      across a machine boundary, multi-shot of such a capture,
      `State.handle` between two prompts, the `NoPrompt` diagnosis at the
      root. They run on `runNested` today and are the oracle
      freer-kont-frames-probe must satisfy.
