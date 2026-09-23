- [ ] polyglot-typescript — stages 1-3 LANDED 2026-09-23: the TypeScript
      worker on the okay wire (ts-worker), TypeScript programs inside okay
      on Scala.js and okay as a JS Promise (ts-scalajs, module okay-ts);
      docs/typescript.md. Remaining, and only when a consumer asks:
      4. okay-js prints TypeScript — PROMOTED 2026-09-23 as ts-js-typed
         (T8 of specs/typescript-types.md).
      5. ScalablyTyped facades for TS libraries used from okay IN THE
         BROWSER: docs name the road, nothing is built or tested. (On the
         BACKEND a TS module has a generated typed Scala facade since
         ts-facade, 2026-09-23: `okay.py.TsFacade`.)
      specs/typescript.md.
