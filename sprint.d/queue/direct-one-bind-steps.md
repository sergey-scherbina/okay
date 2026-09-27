- [ ] direct-one-bind-steps — ORDER 2 of the map-cost plan (operator,
      2026-09-27). The `direct` macro chooses the code it emits, so it can
      write a step as ONE flatMap where the source reads as a map
      followed by a bind (`val y = !op; …`, with the value used through
      a pure function before the next mark). That is the rewrite
      `Free.flatMap` cannot do at run time (map-fusion REFUTED: calling
      the next continuation directly chains Delim's composed
      continuations on the stack). At compile time it is only the
      shape of the tree the macro builds, and every continuation is
      still called by the interpreter. First: read what DirectCompiler
      emits for a straight-line block (okay-direct, src/main/scala/macros)
      and count maps followed by binds in its output for a
      representative block. Then measure a staged vs unstaged block
      (the existing Direct benchmarks) before and after.
