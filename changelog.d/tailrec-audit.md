## tailrec-audit - every loop scalac already made is now a checked `@tailrec`

Survey method: a method that scalac's tail-call elimination turned into
a loop ends with `goto 0` in its bytecode. The scan ran javap over
every JVM main class, mapped each such method back to its `def` by
line number, and listed the ones without the annotation: 133 in okay.

- 68 now carry `@tailrec`, so a future edit that breaks the loop is a
  compile error instead of a silent stack.
- 10 of those 68 are loops that also resume from inside a `flatMap` or a thunk:
  `Producer.each`, `Logic.msplit`, `Effects`' Cont handler, Pipe
  `through`, Frege `list`, the Json/Edn product decoders, jdbc
  `Writes.resolve`, pg `resolve`, and the wroclaw lane. They got a
  one-line `again` wrapper for that call, so `@tailrec` can check them.
- 11 were scanner false positives: `while` loops that do not call
  themselves.
- 54 are recursions only PARTLY in tail position, which scalac
  had half-looped: tree walks through a collection of children, or a
  child visited before a sibling. They stay unannotated here.

The operator rule the same day (AGENTS.md, "NO UNBOUNDED STACK
RECURSION") makes those next, and specs/stack-safety.md holds the
inventory.
