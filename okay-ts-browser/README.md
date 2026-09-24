# okay-ts-browser

A TypeScript library used FROM okay on Scala.js, through a facade
ScalablyTyped generates from the library's own `.d.ts`
(polyglot-typescript stage 5; docs/typescript.md, "TypeScript libraries,
used from okay").

A SEPARATE sbt build, as a user's would be: the converter plugin loads here
only. It depends on okay's JS artifacts as a user's build does, so publish
them first, from the main build:

    scripts/gate.sh "okayJS/publishLocal; okayAsyncJS/publishLocal"

Then, from this directory:

    ../scripts/gate.sh test

`ts-lib/okay-pricing` is the library: a JavaScript implementation and the
`.d.ts` the facade is generated from. It is a local package, so the test
needs no npm registry. After editing its `.d.ts`, run `clean` first: the
conversion is cached against `npmDependencies`, and a `file:` line does
not change when the file behind it does.
