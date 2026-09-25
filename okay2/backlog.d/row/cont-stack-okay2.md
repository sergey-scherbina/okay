- [ ] cont-stack-okay2 — specs/cont-stack.md stage 6: the same three
      layers in the Scala 2.13 core, as far as its macros reach. Measured
      2026-09-25: 20 000 shifts whose bodies call `k` overflow a 128 KB
      stack in okay2 exactly as in okay. Layer 2/3 port straight (a
      `room` parameter and field, the 1 GB platform-thread switch, the
      `ThreadStackSize` first room; okay2 is JVM-only, so no Native or
      JS row, and the FFM reader is the same `jdk22/` Multi-Release
      shape through build.sbt's `versioned` in okay2's own build).
      Layer 1 is where Scala 2 differs: `shift` becomes a blackbox def
      macro — tail bodies (A) and the known higher-order functions are
      in reach; VISIBLE user functions are not (a Scala 2 macro cannot
      read another method's body), so those calls count as opaque; B
      (answer-using bodies) only if it holds up on 2.13 macros, else
      opaque and the spec says so (Open question 2). Every claim with a
      number, per this section's rule; the traps of the Scala 2 port
      memories (implicit list eating the next argument, alias blocking
      row inference) apply to the macro's expansion too.
