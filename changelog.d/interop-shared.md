## interop-shared - one copy of what the JVM-language bridges share

The Java, Clojure and Frege bridges each carried their own copy of four
pieces, and now share one of each:

- `okay.Member[F]` in the core decides whether an `Object` from another
  language is an operation of the row F.
- `okay.Operations` in okay-platform holds the core effects' operations
  as values.
- `okay.Push` in okay-stream drives a `Stage` by pushing its outputs.
  `Gather.gatherer` and `Transducers.of` both run on it.
- `okay.Foreign` in okay-stream walks a program written in another
  language as data, read through a `View` of methods. `Program` and
  `Frege` are each a view plus two one-line calls.

`Frege.Row`, `Program.Row` and the two `Ops` objects keep their names
as aliases and facades, so no call site changed. Four alternating rounds
against the base put every driver within its own noise (the table is in
specs/interop-shared.md). All 122 tests of the three modules still pass,
unchanged. Two mutants on the shared code were each caught by an
earlier lane's test. docs/jvm-languages.md gains a section on bridging a
language it does not cover, with Swierstra 2008 for the idea.
