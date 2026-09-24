## okay2-prog - Prog, the indexed program, and Handler.scala, for the Scala 2 core

`Prog[R, A, S, T]` in okay2 (spec stage 22, docs §26): the Scala 3
core's indexed program — an ordinary `Free` with two phantom indexes,
what holds before and after it runs, so a protocol written as smart
constructors (`begin: Idle -> Open`, `commit: Open -> Idle`) cannot be
called out of order, doubled, or left half done. Zero cost: `diag` and
`free` are the identity and `flatMap` is the same `Bind`; `free` exists
only on the diagonal. A module with an abstract `Rep`, as `Cont` and
`Eager` are. TestProg: the Scala 3 facade tests and a begin/write/commit
protocol. And `Row.scala` is renamed `Handler.scala` (git mv), matching
the Scala 3 core; `trait Row` alone stays in `Row.scala`.
