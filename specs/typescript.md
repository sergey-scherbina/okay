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

## Stage 2 — TypeScript programs inside okay, on Scala.js

A new module, okay-ts (Scala.js only), for TypeScript in the browser or
on Node with no process in between.

- [x] `Ts.run[F, Out](program, callbacks)`: a TypeScript program built
      from the same `done`/`perform`/`then` objects as the worker's
      library, walked by okay IN THE SAME JavaScript runtime. Each named
      operation is an okay callback (`Ts.callback[Arg, Res](name)(f)`) run
      under the caller's handlers. Continuations are JS functions, so a
      `Choice` handler continues one twice (multi-shot, in-process).
- [x] Values cross through okay's JSON codec (`JSON.stringify`/`parse`
      at the boundary), which is exactly the shape `Stubs.typescript`
      declares. A sum is `{ "Case": {...} }`. No cast: the walk reads the
      program through JSON and `js.Dynamic`.
- [x] A JavaScript exception in the program is a `Left(Failure(name,
      message))`, not an escape.
- [x] Stage 3, okay called FROM TypeScript: `Ts.promise(program)` runs an
      `A ! Async` and hands TypeScript a `Promise` of its JSON value.

## Stage 5 — TypeScript libraries used from okay in the browser (ts-browser-facades)

The backlog's `polyglot-typescript` item 5, taken up at the operator's
request (2026-09-24). The docs pointed to ScalablyTyped, and nothing was
built or tested.

- [x] A separate sbt build, `okay-ts-browser/`, depends on okay's JS
      artifacts as a user's build would, and generates a facade from a
      TypeScript library's `.d.ts` (`ScalablyTypedConverterGenSourcePlugin`).
      The converter never loads in okay's own build.
- [x] The library is used from okay programs: a function (its exception
      as a value), a class given a Scala callback under the caller's
      `Reader`, a `Promise` awaited in `Async` (rejection as failure).
- [x] The facade is typed: a wrong argument does not compile
      (`compileErrors`), and changing the library's `.d.ts` breaks the
      Scala code that misuses it (the mutant, after `clean`).

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

- Stages 2 and 3 (ts-scalajs, 2026-09-23). The new module okay-ts
  (Scala.js only) has 5 tests, run by Node through Scala.js in the
  default gate: multi-shot `Choice` over a JS program (11, 21, 12, 22),
  a Reader callback with values in JSON's shape, a sum as
  `{ "Rect": ... }`, a JS `RangeError` and a non-program as `Left`s by
  name, and `Ts.promise` resolving to the JSON value.
  - Mutant: handing each continuation `null` instead of okay's answer
    fails two tests.
  - Found on the way: `JSON.stringify` is typed `String` but answers
    `undefined` for `undefined`, so the match has to be over `Any`.
    Matching over `String` warned that the other case was unreachable.

- Stage 5 (ts-browser-facades, 2026-09-24): `okay-ts-browser/`, with
  sbt-converter 1.0.0-beta45 on sbt-scalajs 1.22 and Scala 3.9. Found
  while building it:
  - The plain and ExternalNpm plugins compile the facades THEMSELVES,
    with a compiler that predates Scala 3.9's standard library
    (`NoSuchMethodError: scala.Option.orNull`, on the `std` facade). The
    GenSource plugin gives the sources to the project's own compiler,
    which is the road that works.
  - `stStdlib := List("es2015")`: the `std` facade is 2229 files with the
    DOM and a handful without it, for a library that needs `Promise`
    only.
  - The conversion is cached against `npmDependencies`. A `file:`
    dependency's `.d.ts` can change without the line changing, so the
    first mutant stayed GREEN until `clean`, even with the package
    version bumped. Documented, not worked around: a published library's
    version is in the line.

