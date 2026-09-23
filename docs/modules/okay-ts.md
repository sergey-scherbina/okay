# okay-ts

TypeScript programs INSIDE okay, on Scala.js, and okay handed to
TypeScript as a `Promise` (specs/typescript.md, stages 2 and 3). Scala.js
only: on the JVM, TypeScript runs as a worker process through okay-py's
`TsWorker` (stage 1).

| | |
|---|---|
| `Ts.run[F, Out](program, callbacks)` | walk a TypeScript program (`done`/`perform`/`then` objects) in the same JS runtime; each named operation an okay callback; multi-shot |
| `Ts.callback[Arg, Res](name)(f)` / `Ts.callbacks(...)` | the named operations, as okay programs in `F` |
| `Ts.promise(program)` | an `A ! Async` as a JS `Promise` of its JSON value |
| `Ts.durable(flow, program, callbacks, journal)` | `run` with each answer journalled first: a reload resumes the flow, a recorded step asked differently is `Drift` |
| `Journal.memory()` / `Journal.indexedDb(name)` | where a durable flow's answers are kept |
| `Ts.toJs` / `Ts.fromJs` | a value through okay's JSON codec, the shape `Stubs.typescript` declares |

The guide: [okay with TypeScript](../typescript.md).
