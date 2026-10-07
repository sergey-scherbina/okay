# okay-std

THE CLASSIC'S EFFECTS: the standard effects written over okay-freer's tree, a module of their own, package
`okay.std`, above okay-freer and depending on nothing else (the operator, 2026-10-08: "все эффекты из
okay-freer перенести в okay-std — само ядро оставь"). Cross-built JVM/JS/Native.

| | |
|---|---|
| `State`, `PState`, `Reader`, `Writer`, `Throws`, `Validated`, `Chronicle`, `Maybe` | state, environment, log, failure and their handlers; `Writer.tracing` records what a program asks for |
| `Choose`, `Logic`, `Prob`, `Dist`, `Sim` | nondeterminism, backtracking search, probability, simulation |
| `Resource`, `Failing`, `Final`, `Provide`, `Module` | scoped resources and the dependency wiring built on them |
| `Once`, `Supply`, `Fresh`, `Random`, `Clock`, `Refs`, `TRef`, `TMap` | by-need values, fresh names, randomness, time, references |
| `Stream`, `Gen`, `Generate`, `Producer`, `Pull`, `Chunk`, `sliding` | streams and generators over the tree |
| `LexicalState`, `Stagers` | State as `Lexical` instances; the stagers of the direct block for these effects |
| `Bisim` | bisimulation of handlers |

`import okay.std.*` (and `okay.std.given` for the instances) beside `import okay.freer.*`. What stays in
okay-freer is the core of the classic: the tree, its handlers and rows, `Shift` and the machine of delimited
continuations, the CPS paramonad. A replay-safe effect's operations extend `Replayable.Safe` (State, Reader,
Throws here; `Shift` in okay-freer).
