- [ ] java-capabilities — operator ask (2026-10-04: "как решить проблему?"
      → "Да"). java-effects gave Java `Eff<A>` with NO static row: a missing
      handler is a run-time refusal. Capability-passing style (Brachthäuser,
      Schuster, Ostermann, "Effects as Capabilities", OOPSLA 2020) makes
      the row the PARAMETERS of a Java method: a handler creates a `Cap<O>`
      and hands it to its body, operations are performed through it and
      split by the capability's IDENTITY — so a program that needs an
      effect does not compile without its handler, and two instances of one
      effect (two states) coexist. Built-ins `Var`, `Env`, `Raise`, `Io`.
      Spec: specs/java-capabilities.md.
