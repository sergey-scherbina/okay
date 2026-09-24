- [ ] okay2-module-plan — `Module.plan`/`exports`/`shadowed` for okay2:
      the Scala 3 core reads the curried chain off `F[Marker]` with a
      macro and generates the collector body. Scala 2 def macros expand
      only across compilation units, so this is a separate `okay2-macros`
      subproject (scala-reflect) the core depends on, which also makes
      `At.here` (the caller's `file:line`, today `<unknown>`) possible —
      one module, two macros. Until then a module's plan is its type,
      readable by a person and not by a program. (2026-09-24)
