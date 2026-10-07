## okay-std: the classic's effects in a module of their own

Lane okay-std (the operator). State, Reader, Writer, Throws, Choose, Logic,
Resource, Provide, Once, Supply, Random, Clock, Refs, Stream, Gen and the rest
move out of okay-freer into okay-std, package `okay.std`; okay-freer keeps the
core (the tree, handlers, rows, `Shift` and its machine, CPS). Code that used
them through `import okay.freer.*` adds `import okay.std.*` (and
`okay.std.given`). Renamed on the way: `Lexical.State` is `LexicalState`,
`Stager.StateWriter`/`All`/`Reading`/`Stateful`/`Logging`/`Failing` are
`Stagers.*`, `!.tracing` is `Writer.tracing`, `!.once` is `Once.once`,
`Reader.RowOf` is `RowOf`; `Replayable.Safe` is a marker trait an effect's
operations extend.
