- [ ] java-effects — operator ask (2026-10-04: "фасад для okay и эффектов
      и хендлеров со стороны java"). okay-java bridges the JDK's TYPES
      (streams, functions, gatherers) but a Java programmer still cannot
      write an `A ! F`, perform an operation, or handle one. Add the
      facade: `okay.java.Eff<A>` (a program over an erased row, as
      okay-scala2's `Eff` stores one at `Top`), `Op<R>` for a Java
      effect's operations, and handlers in the core's forms — answer,
      into, state, control (multi-shot resume) — plus the core effects
      (Reader, State, Throws, Async) reachable from Java. Java has no
      intersection type argument, so the row is not static: `run()`
      refuses an unhandled operation BY NAME. Proven by Java sources in
      okay-java/src/test/java. Spec: specs/java-effects.md.
