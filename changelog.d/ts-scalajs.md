## ts-scalajs - TypeScript programs inside okay, and okay as a Promise

Stages 2 and 3 of specs/typescript.md, in a new Scala.js module,
okay-ts:

- `Ts.run(program, callbacks)` walks a TypeScript program in the same
  JavaScript runtime, with no process. The program is built from the
  same `done`/`perform`/`then` objects the worker uses. A named
  operation is an okay callback under the caller's handlers, and a JS
  continuation can be continued twice, so multi-shot `Choice` works
  in-process.
- Values cross through okay's JSON codec, the shape `Stubs.typescript`
  declares, with no casts.
- A JS exception is a `Left` by name.
- `Ts.promise(program)` hands TypeScript a `Promise` of an okay
  program's value.

Tests: 5, in the default gate, run by Node. A mutant is caught. Docs:
new sections in docs/typescript.md (pinned) and the page
docs/modules/okay-ts.md. backlog polyglot-typescript keeps stages 4–5,
for when a consumer asks.
