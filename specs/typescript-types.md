# typescript-types — one set of types for Scala and TypeScript

## Overview

Operator, 2026-09-23: "сценариев три — скала на бекенде а фронтенд на
тайпскрипте, и второй — скала и тайпскрипт на фронтенде, и третий — скала
и тайпскрипт оба на бекенде. Во всех трёх … чтобы типы были каким-то
волшебным образом одни и те же. Как импортировать и экспортировать
типы? Чтобы их не приходилось писать руками два раза."

| scenario | where Scala runs | where TypeScript runs | how a value crosses |
|---|---|---|---|
| S1 | the backend (JVM) | the browser | HTTP / WebSocket, JSON |
| S2 | the browser (Scala.js) | the browser | in-process, a JS value |
| S3 | the backend (JVM) | the backend (Node worker) | the okay wire |

"The same types" needs three things, and this spec builds them:

1. **ONE SHAPE.** A value must look the same to TypeScript in all three
   scenarios, or no single declaration can describe it. okay's JSON codec
   is that shape: S1 already speaks it, S2 (okay-ts) already speaks it,
   and S3 does not yet (the worker receives okay-py's wire shape, where a
   sum carries a `type` field). Stage T1 makes S3 speak it too, so one
   `.d.ts` serves all three.
2. **ONE SOURCE.** A type is written ONCE, on either side, and generated
   on the other:
   - Scala first: `Schema` → TypeScript (`Stubs.typescript`, exists),
     written to the frontend's tree as a build step (T2).
   - TypeScript first: TypeScript's own compiler reads the declarations,
     and okay writes `case class`/`enum ... derives Schema` (T3).
3. **ONE CHECK.** Where a team keeps both by hand anyway, `tsc` decides
   whether the two are the same type, by mutual assignability, and the
   drift fails a build, not a user (T4).

The same holds for FUNCTIONS, per scenario: S1 a typed API client
generated from okay-http's route declarations (T5); S2 Scala.js functions
exported to TypeScript with their declarations (T6); S3 a TypeScript
module's signatures read by the compiler and written as a Scala facade
(T7).

## Stages

- T1 — **one shape**: `Ts` calls on the JVM (okay-py's `TsWorker`)
  encode and decode through okay's JSON codec, so the worker's values
  are exactly what `Stubs.typescript` declares; the same `.d.ts` checks
  a module in S1, S2 and S3.
- T2 — **Scala → TypeScript, as a build step**: `TsTypes.write(file,
  schemas*)` and a `runMain`, deterministic, header-marked as generated;
  a doc showing the sbt task that regenerates it into a frontend tree.
- T3 — **TypeScript → Scala**: a Node script shipped in the jar, using
  the TypeScript compiler API (`ts.createSourceFile`), reads interfaces,
  type aliases, string-literal unions and discriminated unions, and
  okay writes Scala: `case class`, `enum`, `Option`, `Vector`,
  `derives Schema`. What it cannot map (a function type, a `Map` the
  Schema has no case for) is refused by name, not guessed.
- T4 — **the check**: `TsTypes.check(generated, handwritten)` asks `tsc`
  whether each declared type is assignable to the other both ways; a
  difference is reported by type name.
- T5 — **S1 functions**: a typed `client.ts` (fetch functions, request
  and answer types from the route's `Schema`s) generated from okay-http's
  declared routes.
- T6 — **S2 functions**: `Ts.export(name)(f)` publishes an okay function
  to TypeScript on Scala.js, with its generated declaration
  (`export function name(a: A): Promise<B>`).
- T7 — **S3 functions**: a TypeScript module's exported signatures, read
  by the compiler, written as a Scala facade (the TypeScript twin of
  `PyFacade`).

Each stage is a lane; Results below record what each found.

## Stage T1 — one shape

- [x] `Ts.fn[Out]("mod:f")(args)`, `Ts.callback`, `Ts.hold`,
      `Ts.program` on the JVM (package `okay.py`, object `Ts`): the same
      operations as `Py.*`, but every value is encoded by okay's JSON codec
      and handed to TypeScript as the plain object JSON would parse to; an
      answer is decoded by the same codec.
- [x] A sum crosses as `{ "Case": {...} }`, `None` as `null`, a `BigInt`
      as a string of digits — the `Stubs.typescript` shapes, asserted
      against the real worker.
- [x] ONE declaration file checks all three: a TypeScript module typed
      with `Stubs.typescript` runs in the worker (S3), and the same types
      describe what okay-ts hands a program in the browser (S2) and what an
      okay HTTP endpoint answers (S1); `tsc --strict` passes.
- [x] `Stubs.typescriptWire` stays for callers of `Py.*` on a TS worker;
      docs/typescript.md says to prefer `Ts` and `Stubs.typescript`.

## Stage T6 — Scala.js functions, TypeScript callers (S2)

- [x] `Ts.expose[A, B](name)(f: A => B ! Async)` is one function a
      TypeScript caller awaits: its argument is read with okay's JSON codec,
      its answer written with it, and a wrong argument REJECTS the promise,
      naming the function and the reason.
- [x] `Ts.module(name)(exposed*)` gives two things: `.js`, the object to put
      behind `@JSExportTopLevel`, and `.declaration`, the declaration
      file TypeScript reads for it. That file holds `Stubs.typescript`'s
      types for every argument and answer, and
      `export declare const name: { f(input: A): Promise<B>; ... }`.
- [x] `tsc --strict` accepts a TypeScript caller written against the
      declaration and refuses a wrong field. The declaration is generated
      by the same code that encodes the values, so the two cannot drift.

## Stage T7 — a TypeScript module as a Scala facade (S3)

- [x] `TsFacade.declarations(dir, module)` runs
      `tsc --declaration --emitDeclarationOnly` on the module. The
      compiler, not a guess, answers each function's types, including a
      return type the source did not write.
- [x] `TsTypes.parseModule` reads those declarations: the data subset
      T3 reads, plus `export declare function f(a: T, b?: U): R;`. The
      names a `import type { … }` brings in are known types (they
      typically came from Scala through `Stubs.typescript`).
- [x] `TsFacade.render(obj, pkg, module, dts, imports)` is Scala source.
      It holds the module's own data types (T3's output) and an object
      with one method per function, each calling it through `Ts.fn`
      in the JSON shape:
      - a `Promise<T>` answers `T`, since the worker awaits it;
      - `unknown`/`any`/`void` are TYPE PARAMETERS (`Out: Schema`,
        `A: ToPy`), as in PyFacade;
      - an optional parameter is left out and named in the comment;
      - a function the facade cannot type is a comment saying why,
        never a guess.
- [x] Live: the facade of a real module is checked in, and a test
      asserts the generator writes it unchanged; its methods call the
      TypeScript worker and answer typed values.

## Decisions

- **The JSON codec's shape is the one shape**, not the wire's. It is what
  every HTTP client already sees (S1), what Scala.js hands JavaScript most
  cheaply (`JSON.parse` of the codec's text, S2), and what a TypeScript
  developer expects of a JSON API. The wire's shape (a `type` field) suits
  Python's `TypedDict`, and it stays there.

## Results

- T1 (ts-one-shape, 2026-09-23).
  - A `Shape` (python | json) is now a parameter of okay-py's call
    classes. `Py` keeps `Shape.python` as the default given, and `Ts` is
    the same API with `Shape.json` in lexical scope. A held object
    remembers the shape it was made with, so its methods speak it too.
  - TestTsOneShape has 4 live tests. The worker receives exactly the JSON
    an HTTP endpoint sends (compared as parsed JSON, and literally for a
    sum: `{"Rect":{"w":2,"h":3}}`). A call typed only by
    `Stubs.typescript` covers an `Option`, a callback and a sum both
    ways. Multi-shot works in the JSON shape. `tsc --strict` compiles the
    module and refuses a wrong field.
  - All 22 unit tests and 66 live okay-py tests still pass.
  - Mutant: `Ts` with the Python shape fails two tests.
  - Found on the way: the test's object-level `val` summoning
    `Schema[Order]` for a case class nested in that object DEADLOCKED one
    thread (a Scala 3 lazy val is not re-entrant). The gate's watchdog
    caught the stall and its dump named it. It is a `def` now.

- T2 (ts-types-scala-to-ts, 2026-09-23). `okay.codec.StubFiles`
  (JVM) has `typescript` and `python` (write the generated declarations)
  and `write` (only when the text changed; answers whether it wrote).
  TestStubFiles has 2 tests: written once, left alone while unchanged
  (same mtime), rewritten when a type is added; and the Python twin. The
  build step is a user's `main` listing the types plus an sbt task. Its
  shape is shown in the doc and not tested here, because it belongs to
  the user's build.
  - Mutant: always rewriting fails "left alone while the model does not
    change".

- T3 (ts-types-ts-to-scala, 2026-09-23). `okay.codec.TsTypes` is a
  parser of the data subset of TypeScript declarations (tokens, then
  recursive descent) and a Scala writer. It is pure Scala, so it runs on
  every platform, and needs no Node. The installed TypeScript is 7, the
  native port, and has no compiler API to load.
  - `Stubs.typescript` now names the leaves (`Int`, `Long`, `Char`,
    `BigIntDigits`, `Base64`), emitting only the aliases a file uses, so
    the round trip is exact.
  - TestTsTypes has 5 tests (all platforms):
    - Scala → TS → Scala gives a golden file that compiles;
    - that file's TS equals the original byte for byte;
    - a hand-written TS model becomes a case class, an `Option` field and
      an enum;
    - the wire's sum shape reads as the same enum;
    - six refusals by name, with the line where the source has one.
  - The live `tsc` tests (TestStubsTsc, TestTsOneShape, TestTsWorker) and
    TestPyStubs all pass with the new aliases.
  - Mutant: reading `Long` back as `Double` fails the round trip.
  - A refusal message was sharpened on the way: `Record<…>` was called
    "generic" and is now "a map is not read; a Schema has no map case".

- T4 (ts-types-check, 2026-09-23). `okay.codec.TsCheck` (JVM) writes the
  two copies and a `check.ts` holding one line per type,
  `Same<G.X, H.X> = true`, and runs `tsc --strict`. Each error names its
  line, and so its type.
  - The leaf aliases (`Int`, `Long`…) are left out of the comparison. The
    first run compared `Int`, and a hand-written copy that says `number`
    has no `Int` to compare. `Same` compares THROUGH the aliases, which
    is the point.
  - TestTsCheck has 4 live tests: another order and format are the same;
    a renamed field, a field made optional, and a missing type are each
    named.
  - Mutant: a one-way `Same` misses the optional field, and the test
    fails.

- T5 (ts-api-client, 2026-09-23).
  - A router entry now carries its Scala types beside their JSON Schemas
    (`Entry.bodyType`, `Answer.tpe`), filled by the constructors that
    decode or encode (`json`, `out`, `jsonOut`, and their `At` and
    headed forms). `Stubs.typescriptType` gives the TypeScript expression
    for one schema.
  - `okay.http.TsClient.model/client` write `model.ts` and `client.ts`.
  - TestTsClient (Live) starts a real server and a Node program that
    creates, reads and filters through the generated client and gets a
    400 for a bad body. `tsc --strict` accepts that program and refuses a
    wrong field. TestTsClientShape (default gate) pins the signatures.
  - Mutant: a client that does not fill in the path parameter fails the
    end-to-end test.
  - Two test-side findings: query parameters are not part of a
    function's name (`getTagged`, not `getTaggedTag`, which my own test
    first assumed); and `tsc` needs `"type": "module"` in a
    `package.json` for top-level `await`, which Node had guessed from the
    syntax.
  - The full matrix caught what the JVM run could not: the first cut split
    the path with a look-behind regex, and Scala.js refuses look-behind
    below ES2018. The same generator runs on JS for a browser-side
    build, so it is a scan now, and the shape test runs on both
    platforms.

- T6 (ts-export, 2026-09-23).
  - `Ts.expose` and `Ts.module` exist in okay-ts; `TsModule.js` is typed
    `js.Object & js.Dynamic` (a `Dynamic.literal`), so no cast was
    needed.
  - TestTsExport (Scala.js, on Node) checks four things:
    - a call gives the codec's shape;
    - a missing field rejects with a `TypeError` that names the
      function;
    - the declaration text is pinned;
    - (Live) `tsc --strict` accepts a caller and refuses `t.price`. tsc
      runs through Node's `process.getBuiltinModule("child_process")`,
      since okay-ts links without a module system (no `require`).
  - Mutant: declaring the answer with the argument's type fails both the
    pinned declaration and tsc.
  - Two naming findings:
    - `export` is a hard keyword in Scala 3, so the spec's `Ts.export`
      is `Ts.expose`;
    - a class named `Module` inside `object Ts` shadows `java.lang.Module`
      (E177), so it is `TsModule`.

- T7 (ts-facade, 2026-09-23).
  - `TsTypes.parseModule` reads exported functions. A function it cannot
    type becomes an `Unread` with the reason, instead of refusing the
    module. `TsTypes.scalaType` and `renderData` are the type mapping,
    shared with T3.
  - `TsFacade.declarations` and `render` are in okay-py.
  - The facade of `golden/facadets.ts` is checked in (`FacadeTs.scala`).
    TestTsFacade (Live, tsc + Node) checks three things:
    - the generator writes that file unchanged;
    - its methods call the worker (a case class from Scala's model, an
      async function whose answer type only tsc knew, an optional
      parameter left out, an open type);
    - a module tsc refuses comes back with tsc's reason.
  - TestTsFacadeRender (default gate) covers the reading and the imports
    on handwritten declarations.
  - Mutant: without Promise unwrapping, the golden differs.
  - A finding: the parser reported a callback parameter
    (`f: (x: number) => number`) as "expected ')', found ':'". It now
    recognises an arrow's parameter list and says "a function type is
    not data".
  - Refuted alternative: a TypeScript compiler API walk. TS 7 (the
    native port) ships none. tsc's own declaration output, read by the
    T3 parser, gets the inferred types without it.
