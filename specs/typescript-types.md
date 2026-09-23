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
- T8 — **Scala CODE → TypeScript**: `Direct.ts { … }` prints the okay-js
  subset as TypeScript, each `var` and each function parameter annotated
  with the type the Scala compiler gave it.
- T9 — **okay on npm**: okay-ts linked as an ES module and packed with
  its `.d.ts`, so a TypeScript project installs okay; CRDT replicas and
  okay streams (as `AsyncIterable`) exported through it.
- T10 — **durable flows in the browser**: a TypeScript program's
  answers journalled in IndexedDB, so a wizard resumes after a reload.
- T11 — **a live frontend, typed by path**: the dotted paths of a
  `Watched` document as TypeScript types generated from its Schema, and
  a client that subscribes to them.

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

## Stage T8 — Scala code as TypeScript (okay-js)

- [x] `Direct.ts { … }` reads the same closed subset as `Direct.js`
      and gives a tree whose `var`s and function parameters carry their
      types. The type is the one the Scala compiler inferred:
      - `Int`/`Long`/`Double`/… is `number`, `String` is `string`,
        `Boolean` is `boolean`;
      - a `Dyn` or a spliced `Js` is `any`, which is honest: it is
        untyped JavaScript;
      - a Scala function type is `(a: A) => B`.
      A type outside that list is refused by name.
- [x] `Direct.tsSource { … }` is the TypeScript text as a compile-time
      constant, like `Direct.source`.
- [x] `Js.print` prints the typed tree as JavaScript with the types
      dropped, and `Js.printTs` keeps them. A typed tree runs as the
      same JavaScript.
- [x] (Live) `tsc --strict --noEmit` accepts the printed TypeScript,
      and refuses it when a Scala type was wrong for the value.

## Stage T9 — okay on npm

- [x] An npm package directory (`package.json`, an ES module linked
      by Scala.js, a hand-checked `index.d.ts` written from the same
      Schemas):
      - `run(program, callbacks)` walks a TypeScript program;
      - a CRDT replica type (counter, register, set) is exported with
        its merge;
      - an okay stream is exported as an `AsyncIterable`.
- [x] (Live) `npm pack`, then `npm install` of the tarball into a fresh
      TypeScript project, offline. `tsc --strict` compiles a consumer,
      and Node runs it.

## Stage T10 — durable flows in the browser

- [x] `Ts.durable(key, program, callbacks, journal)` walks a
      TypeScript program and records each answer, keyed by the flow and
      the step, before the program continues.
      - A replay hands recorded answers back without calling the
        callback again.
      - A replay whose step asks a different name or different
        arguments is refused (drift), not silently answered.
- [x] `Journal` has two implementations: in memory, and IndexedDB.
- [x] (Live) In a real headless Chrome with one profile directory, a
      flow run in two page loads, killed between them, finishes with
      each callback called exactly once.

## Stage T11 — a live frontend, typed by path

- [ ] `Stubs.typescriptPaths(schema, name)` writes an interface
      `NamePaths` mapping every dotted key `JsonOptic.path` accepts to
      the TypeScript type of what it focuses. An array index is a
      template-literal key: `` `tasks[${number}]` ``. Recursion is cut
      at a stated depth.
- [ ] A TypeScript client
      `watch<K extends keyof NamePaths>(key: K, f: (v: NamePaths[K]) => void)`
      and `set(key, value)`, over a `Watched` served by okay-http.
      (Live) `tsc` refuses a path the schema does not have, and Node
      sees the pushes.
- [ ] A framework-free custom element `<okay-live>` and a React hook
      over the same client. The React part is typechecked only where
      `@types/react` is installed, and says so.

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

- T8 (ts-js-typed, 2026-09-23).
  - `Direct.ts`/`tsSource` exist, and `Js.printTs` prints the tree. There
    are two new typed nodes, `Stmt.TypedVar` and `Js.TypedFun`. The
    printer is one class with a `ts` flag, so the JavaScript and the
    TypeScript differ only in the types.
  - TestDirectTs runs in the default gate on JVM, JS and Native.
    TestDirectTsc (Live) has `tsc --strict` accept a printed program and
    refuse a string under `number`.
  - Mutant: dropping the parameter types fails the typed test.
  - Two defects of `js { }` found on the way. Both were in
    `Direct.js` before T8, and both printed JavaScript that runs and does
    the wrong thing:
    - `f(1)` on a function val printed `f.apply(1)`. The typed test's
      expected text found it: the untyped suite never called a
      function value.
    - `d.f = v` on a Dyn printed `d.updateDynamic("f", v)`. tsc found it:
      "Property 'updateDynamic' does not exist on type 'Document'". A
      type checker on the output is a test of the translator too.
  - A function's return type is deliberately not annotated. A
    `js { }` lambda's body is a statement, so the function returns
    `undefined`, and annotating Scala's result type would be a claim tsc
    rejects.

- T9 (ts-npm, 2026-09-23).
  - `okay-ts-npm` is a Scala.js ES module, the one ESModule-linked
    project in the build. `npmPackage` writes target/npm.
  - Its `index.d.ts` is the module's own `declarations` export: the CRDT
    state types come from `Stubs.typescript` over okay-crdt's Wire
    schemas, and the signatures are written beside the exports. Nothing
    generated can drift, and the hand-written part is held by tsc on the
    real module.
  - `Ts.runJson` joins `Ts.run` (one walk, two finishes), since a package
    caller's program answers any JSON.
  - TestOkayNpm (Scala.js, default gate, 7 tests) covers:
    - a synchronous and a Promise callback;
    - a throwing callback rejecting the run;
    - GCounter merge in both orders;
    - an OR-Set keeping a concurrent add;
    - a wrong state refused with a named TypeError;
    - a channel read to its end through `Symbol.asyncIterator`;
    - the declarations naming every export.
  - `scripts/ts-npm-check.sh` (Live; GREEN under sh and bash):
    - `npm pack`, then an offline `npm install` into a fresh project;
    - `tsc --strict` accepts the consumer and refuses a `number` read as
      a `string`;
    - Node prints the expected line.
  - Mutant: declaring `gcounter.value` as `string` turns the check RED
    at tsc.
  - A finding in the build: `node -e` with a dynamic `import()` of the
    linked module never settled (Node 26, exit 13, "unsettled top-level
    await"), while a static import of the same file loads at once. The
    task writes a small .mjs file instead.
  - Not done, on purpose: publishing to the npm registry, which is the
    owner's outward step.

- T10 (ts-durable-browser, 2026-09-23).
  - `Ts.durable`/`durableJson` and `Journal` (memory, IndexedDB) are in
    okay-ts. `durable`, `memoryJournal` and `indexedDbJournal` are in
    `@okay/ts`, where a journal is any object with Promise-returning
    `load`/`append`/`clear`.
  - An entry is `{name, args, answer}`, so a replay checks the question
    before it is handed the answer (Drift).
  - TestTsDurable (Scala.js, memory journal) covers four cases:
    - replay without calling a callback;
    - resume after a death at step 2, with step 1 not repeated;
    - drift refused with its step named;
    - clear starting the flow afresh.

    TestOkayNpm adds `durable` through the package.
  - `scripts/ts-durable-browser-check.sh` (Live, GREEN under sh and
    bash) uses real headless Chrome 154 and IndexedDB: two loads of one
    profile, the first dying at the charge, the second finishing with
    `{"reserve":1,"charge":1}`.
  - Mutant: without the replay, three memory tests fail, and Chrome sees
    `reserve: 2`.
  - Three findings on the way to a browser check that works:
    - `--dump-dom` dumps at the load event, before a module's async work.
      Under `--virtual-time-budget` it HUNG with IndexedDB pending.
    - A loopback server started from the tool's shell was not reachable
      (curl answered 000), so the page is a file:// document.
    - Inlining the Scala.js module does not work: it exports under
      internal names (`export { $e_x as x }`), so the names are not
      bindings of the script that holds them. The page imports the
      module from a data: URL.

    `scripts/chrome-read.mjs` drives Chrome through the DevTools
    protocol (Node 26's WebSocket, no dependency). It deletes a stale
    `DevToolsActivePort`, which a second load of the same profile
    otherwise reads as the first browser's port.
