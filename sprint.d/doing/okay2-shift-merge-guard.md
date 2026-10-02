- [ ] okay2-shift-merge-guard — the core's shift-merge-guard (83a0b7084) in
      the Scala 2.13 twin (operator: "продолжай", after okay2-shift-merge):
      ONE evidence `Shift.Machine[F]` for "a machine already runs in F",
      `OneMachine`/`NoMachine`/`Nesting` gone; every machine-starting door
      nests on a running machine instead of refusing; an abstract row is a
      compile error asking for the evidence as a parameter. (2026-10-02)
