- [ ] polyglot-typescript — stage 1 LANDED 2026-09-23 (ts-worker): a
      TypeScript worker on the okay wire, with typed calls, callbacks,
      held objects and multi-shot programs as data; `Stubs.typescriptWire`
      checked by tsc; docs/typescript.md. What remains, in order:
      2. TS programs INSIDE okay on Scala.js: the same done/perform/then
         objects walked by `okay.Foreign` in the browser or on Node, with
         no process (a `Foreign.View` over JS objects, tested on Scala.js).
      3. okay CALLED from TS: an `@JSExportTopLevel` facade (run a
         program, get a Promise) with its `.d.ts` from `Stubs.typescript`.
      4. okay-js prints TS (type annotations on the typed tree).
      5. ScalablyTyped for consuming TS libraries: a docs page.
      specs/typescript.md.
