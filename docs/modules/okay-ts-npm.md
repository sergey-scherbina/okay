# okay-ts-npm

okay as an npm package for TypeScript projects, `@okay/ts`
(specs/typescript-types.md, stage T9). It is okay-ts, okay-crdt and
okay's channels, compiled by Scala.js into one ES module. The package's
`index.d.ts` is written by the module itself, from the same Schemas that
encode its values.

| | |
|---|---|
| `run(program, callbacks)` | walk a program built with `then`/`performing`/`done`; a callback answers a value or a Promise |
| `gcounter`, `pncounter`, `orset` | CRDT replicas as plain JSON states, each with `merge` |
| `channel()` | an okay `Channel`: `offer`, `close`, and `for await` |
| `declarations` | the package's `index.d.ts`, as text |

Build the package directory with `sbt okayTsNpmJS/npmPackage`, into
`okay-ts-npm/.js/target/npm`. `scripts/ts-npm-check.sh` checks it the way
a user meets it:
1. `npm pack`;
2. an offline install into a fresh project;
3. `tsc --strict`;
4. Node.

The guide: [okay on npm](../typescript.md#okay-on-npm-okayts).
