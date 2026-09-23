- [ ] polyglot-typescript — NOT SUPPORTED TODAY, said plainly:
      "TypeScript" appears only as a language okay-rag indexes. What
      exists for JavaScript is okay itself cross-built to Scala.js (okay
      programs run in the browser and on Node) and okay-js, which EMITS
      JavaScript as a typed tree. Operator, 2026-09-23: "А что насчет
      тайпскрипта? Что нибудь можно сделать?" — yes, in this order:
      1. TS PROGRAMS AS DATA, in the browser/Node, on Scala.js. Two
         spellings a TS author already knows: a generator
         (`function* () { const env: number = yield ask(); yield tell(env) }`,
         typed `Generator<Op, R, Answer>`) — ergonomic, ONE-SHOT; and a
         `step(op, k)` builder (okay.core's shape in TS) — MULTI-SHOT,
         because `k` is a plain function. Both walked by `okay.Foreign`
         (interop-shared put it in okay-stream's shared sources, so it
         already runs on Scala.js); a View over `iterator.next(answer)`
         and one over `{op, k}` objects.
      2. OKAY CALLED FROM TS, TYPED. An `@JSExportTopLevel` facade module
         (okay-java's idea on Scala.js: run a program, get a `Promise`)
         and a GENERATED `.d.ts` — Scala.js emits none, so we generate it
         from the facade's export list and from `Schema` for the data
         types (with polyglot-schema-stubs). Published as an npm package
         whose types a TS compiler checks.
      3. OKAY-JS PRINTS TS. The typed tree gains type annotations and a
         `.d.ts` printer, so `Direct.js { }` output can be `.ts`.
      4. ON THE JVM: GraalJS in-process (TS transpiled first — esbuild or
         `tsc`), the same View over polyglot `Value`s, sharing the
         embedding code with py-graalpy-engine; or Node/Deno as a
         subprocess over polyglot-remote-foreign.
      5. CONSUMING TS libraries from okay-on-Scala.js: ScalablyTyped
         facades — a docs page, nothing to build.
      Spec first (specs/typescript.md); stage 1 is the smallest thing
      that proves the claim: a TS generator performing Reader and State
      under okay's handlers, in a Scala.js test.
