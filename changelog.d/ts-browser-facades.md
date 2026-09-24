## ts-browser-facades - TypeScript libraries used from okay in the browser

polyglot-typescript stage 5, the last open item of that backlog entry
(which is now closed): the docs named ScalablyTyped as "a pointer, not a
promise". It is built and tested now.

- `okay-ts-browser/`, a separate sbt build (as okay2 is), so the
  converter plugin never loads in okay's own. It depends on okay's
  published JS artifacts, as a user's build does. A local TypeScript
  library, `okay-pricing` (`.js` and `.d.ts`), goes through a facade
  ScalablyTyped generates from the `.d.ts`. It is used from okay programs:
  a function whose exception is a value, a class with a Scala callback
  under the caller's Reader, and a Promise awaited in Async (a rejection
  is the program's failure).
- The facade is typed. A wrong argument does not compile, and a changed
  `.d.ts` breaks the misusing Scala code after `clean`.
- Found and documented: the GenSource plugin is the road that works on
  Scala 3.9, since the other plugins compile facades with a compiler that
  predates its standard library; `stStdlib := es2015` keeps the standard
  facade small; and the conversion is cached against `npmDependencies`.
- Docs: docs/typescript.md, "TypeScript libraries, used from okay"
  (examples pinned to the build's sources); okay-ts-browser/README.md.
  Spec: specs/typescript.md stage 5.
