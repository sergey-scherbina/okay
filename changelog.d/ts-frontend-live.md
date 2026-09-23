## ts-frontend-live - a live frontend, typed by the document's Scala Schema

T11 of specs/typescript-types.md.

- `Stubs.typescriptPaths(schema, name)` writes `<Name>Paths`: every key
  `JsonOptic.path` accepts, mapped to the TypeScript type of what it
  focuses. A list index is a template-literal key, and a possible absence
  is `| null`. A law test checks that every declared key is accepted by
  the path.
- `LiveHttp.routes(watched)` (okay-live) serves a `Watched` document:
  - server-sent events for `watch`, starting with the current value;
  - `POST set`, answering 204, 409 where the place is absent, or 400.
- `LiveHttp.client` is the TypeScript client: `live<Paths>()` with a
  typed `watch`/`set`, and the `<okay-live>` custom element.
  `LiveHttp.react` is a `useWatch` hook.
- End to end, Node against a real server: the watch sees the value now
  and the written one. `tsc --strict` refuses an unknown path and a
  wrongly typed value. A mutant is caught.

Docs: "A live frontend, typed by path" in docs/typescript.md.
