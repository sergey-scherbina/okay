- [ ] java-direct-style — PRIORITY: LOW (trigger). From java-direct-probe
      (2026-10-06, specs/java-direct-effects.md). A DIRECT-style Java
      effects module: `throws` as the static row (exact while each handler
      leaves one unknown effect, a written witness beyond) and
      `jdk.internal.vm.Continuation` as the resumption, measured 104 ns an
      operation against `Free`'s 14.3 (7.3x). One-shot forms only (answer,
      state, into, abort WITH a discontinue so `finally` runs); opt-in, as
      every user needs `--add-exports java.base/jdk.internal.vm=ALL-UNNAMED`.
      Virtual threads are not the machine: 1.5 µs at best (105x).
      TRIGGER: a Java user for whom `Eff`/`Cap`'s `flatMap` style is the
      obstacle, or the JDK exporting a continuation API.
