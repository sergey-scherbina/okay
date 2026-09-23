## ctx-reader-bridge - the two lines every call site was writing, in the library

The operator lifted the wait ("сделай без триггера. Это полезная
штука, пусть будет", 2026-09-23). The bridge itself had been settled
on 2026-09-01 (E10): a `Conversion` cannot do it — a context function
auto-applies before a conversion could see it — so it is two named
functions, and they lived as local `def`s inside `TestCtxReaderElim`.

- `Reader.lift(cf: E ?=> A): A ! Reader % E` — asks once, applies.
- `Reader.unlift(p: A ! Reader % E + F): E ?=> A ! F` — runs under
  the ambient `E`, forwarding the rest of the row.
- `TestReaderBridge` (core, 3): nothing runs at `lift`, exactly one
  `Ask` per lift (counted by stepping), `unlift` forwards a `Writer`,
  and both round trips. `TestCtxReaderElim` calls the library's lines
  where it had its own.
- specs/context-functions.md's filing updated; guide §9 gains the
  paragraph.

Gate `affected master` green, no warnings.
