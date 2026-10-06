## okay-cont: the delimited-continuation machine, and the freer monad as a module below the core

Feature, branch feature/freer-min (specs/freer-min.md, stages 1–30). A new
module `okay-cont` (package `okay.cont`): a monad of delimited
continuations typed by two stacks of answer types, `Cont[I, O, A]`, seven
nodes, no cast, no prompt, no row — `shift0`/`reset` with answer-type
modification, handlers as delimiters (deep), operations as one capture
through the delimiters between, tail-resumptive handlers answered in place,
the handler found in the context's types at compile time. `Free[R, A]` with
nominal list rows over it (stage 27), `Prog` as an `Effects` encoding of the
machine chosen by the given (stage 30). JMH against the core's handlers:
general clauses 1.29x, answering 1.0x, tail calls 0.85x (stage 28).

Stage 29 moves the freer monad out of the core: `Freer` to package
`okay.freer` in module `okay-freer`, the type classes of Monad.scala with it
(still package `okay`). The core keeps the name at its door
(`src/main/scala/Free.scala`, an alias by a stable path) and its effect tree
`Free` at `Unary`; the doors `p.handle`, `p.run` and the direct colouring
move from `Freer`'s companion to `Diagonal`'s, in the implicit scope of
every `A ! F` — so nothing written against `okay.*` changes, with or
without a wildcard import. `okay-direct`'s macros name the symbols by their
home. Module rule (specs/modules-infra.md): `okay-freer` is the
dependency-free one, the core depends on it alone.
