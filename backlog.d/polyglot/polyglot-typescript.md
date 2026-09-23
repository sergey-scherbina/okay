- [ ] polyglot-typescript — stages 1-3 LANDED 2026-09-23: the TypeScript
      worker on the okay wire (ts-worker), TypeScript programs inside okay
      on Scala.js and okay as a JS Promise (ts-scalajs, module okay-ts);
      docs/typescript.md. Remaining, and only when a consumer asks:
      4. okay-js prints TypeScript: type annotations on the typed tree,
         so `Direct.js { }` output can be `.ts`.
      5. ScalablyTyped facades for TS libraries used from okay IN THE
         BROWSER: docs name the road, nothing is built or tested. (On the
         BACKEND a TS module has a generated typed Scala facade since
         ts-facade, 2026-09-23: `okay.py.TsFacade`.)
      specs/typescript.md.
