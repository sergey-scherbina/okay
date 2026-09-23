## ts-npm - okay as an npm package for TypeScript projects

T9 of specs/typescript-types.md.

- `okay-ts-npm` is okay-ts, okay-crdt and okay's channels as one Scala.js
  ES module. `sbt okayTsNpmJS/npmPackage` writes the package: the
  module, `package.json`, and an `index.d.ts` the module writes of
  itself from the same Schemas that encode its values.
- Exports:
  - `run`, `then`, `perform`, `performing`, `done`: programs as data,
    with sync or async callbacks;
  - `gcounter`, `pncounter`, `orset`: CRDT replicas as JSON states with
    `merge`;
  - `channel()`: an `AsyncIterable`.
- `Ts.runJson` is added to okay-ts.
- `scripts/ts-npm-check.sh` packs, installs the tarball offline into a
  fresh project, has `tsc --strict` accept a consumer and refuse a wrong
  one, and runs it with Node. A mutant is caught.

Not published to the npm registry: that is the owner's step.

Docs: "okay on npm: `@okay/ts`" in docs/typescript.md.
