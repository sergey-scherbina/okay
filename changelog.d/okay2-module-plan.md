## okay2-module-plan - `Module.plan`/`exports`/`shadowed` in okay2

The Scala 3 core's DI plan, rewritten as a Scala 2 blackbox def macro
(`okay2/src/main/scala/okay2/ModuleMacro.scala`):
- `plan` reads the curried `Function1` chain off the module's type,
  outer to inner, and builds nothing.
- `exports` generates the typed collector body and gives name, erased
  class and value for each capability.
- `shadowed` names capabilities installed twice.

An alias role (`type Primary = RDb`) keeps its own name and exports
under its underlying class. This is okay2's version of the core's
opaque qualifier.

The macros live in the core, not in the `okay2-macros` subproject the
backlog item assumed: a def macro is expanded at its use site. 5 new
TestModule tests (specs/okay2.md stage 38).
