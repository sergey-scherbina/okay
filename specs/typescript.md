# typescript — TypeScript with okay

## Overview

Operator, 2026-09-23: "Делай typescript". What existed for JavaScript was
okay itself cross-built to Scala.js, and okay-js, which EMITS JavaScript
as a typed tree. TypeScript code could not talk to an okay program at
all. Two roads, one lane each:

1. **A TypeScript WORKER on the okay wire** (this file's stage 1). Node
   runs `.ts` directly (it strips the types; Node 26 here), so a TypeScript
   process can speak the same line protocol as okay-py's Python shim and
   the Haskell worker. Everything the wire carries then works for it,
   through the API okay-py already has: typed calls, callbacks into okay's
   effects, held objects, and programs as data, multi-shot included.
2. **TypeScript programs inside okay on Scala.js** (stage 2): in the
   browser, with no process. The same `done`/`perform`/`then` program,
   walked by `okay.Foreign` in the same JavaScript runtime.

## Stage 1 — the worker

The jar ships two files under `/okay/ts/`:

- `okay.ts`, the LIBRARY a user's module imports: `done`, `perform`,
  `then` (programs as data), `call` (a callback into okay), and the
  `Value` type.
- `worker.ts`, the PROCESS: it loads the user's modules, speaks the wire,
  and keeps held objects and continuations.

`TsWorker.start(dir, modules)` writes both beside the user's `.ts` files
and runs `node worker.ts`; the engine is `PySubprocess.speaking`, so
`Py.fn`, `Py.callback`, `Py.hold`, `Py.program` and `Durable` drive it
unchanged.

### Behavior

- [x] A typed call: a case class reaches TypeScript as a plain object, and
      the answer comes back through `Schema`.
- [x] A callback: `call("price_of", sku)` inside a TypeScript function runs
      an okay program under the caller's handlers, synchronously (the
      worker reads its stdin with `readSync`, so the function simply
      returns the value).
- [x] An async TypeScript function (a `Promise`) is awaited.
- [x] A held object: its methods and fields, and the object as an argument.
- [x] Programs as data, MULTI-SHOT: `then(perform("choose", [1, 2]), x => ...)`
      under okay's `runChoice` answers every branch.
- [x] Integers past 2^53 cross as `bigint`, bytes as `Uint8Array`, and
      NaN stays NaN.
- [x] `Stubs.typescriptWire(schemas*)`: the `.d.ts` of the shapes the
      WIRE hands TypeScript (a sum carries `type: "Case"`, as in Python;
      a `Long` or `BigInt` is `number | bigint`, since only a value past
      2^53 arrives as a bigint). A user module typed with it passes
      `tsc --strict`, and a read of a field that does not exist fails.
- [x] A TypeScript exception is a condition by name; the worker lives on.

## Decisions

- **Node's own TypeScript, no build step.** Node runs `.ts` by stripping
  types, so the worker needs neither `tsc` nor a bundler at run time.
  `tsc` is used only by the TESTS, to check the types.
- **Modules loaded at start.** A callback's nested request is served
  synchronously from inside the waiting function, and a dynamic `import`
  is asynchronous. So the modules are imported once, at start, named in
  `OKAY_TS_MODULES`, the way okay-r loads its inline modules.

## Results

- Stage 1 (ts-worker, 2026-09-23), with Node 26.9 and tsc.
  - A hand-run smoke test of the worker answered every operation on its
    first run.
  - TestTsWorker has 7 live tests: a typed call whose TypeScript calls
    back into okay's Reader; a sum with its type field; an awaited async
    function; a held `Counter`; multi-shot `Choice` over a TypeScript
    program (11, 21, 12, 22); `Long.MaxValue` as a bigint, bytes and NaN
    round trips; a `RangeError` as a condition by name, with the worker
    alive after it; and `tsc --strict` accepting the module typed with
    `Stubs.typescriptWire`, while refusing `order.skuu`.
  - Default gate: the wire declarations' shape. Mutant: a sum case without
    its `type` field fails it.
  - Not checked with `tsc`: the worker itself. It needs `@types/node`,
    which is not installed; Node runs it, and the tests exercise it.
