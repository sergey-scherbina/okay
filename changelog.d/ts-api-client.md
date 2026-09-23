## ts-api-client - a typed TypeScript client generated from okay-http routes

T5 of specs/typescript-types.md: a Scala backend and a TypeScript
frontend.

- Router entries now carry their Scala body and answer types.
- `TsClient.model(router)` writes the types, and
  `TsClient.client(router)` writes one async function per route
  (`getTasksById(o, id): Promise<Task>`,
  `postTasks(o, body: NewTask)`, `getTagged(o, { tag })`).
- Path, query and body are typed from the route's own declaration, and
  the answer is typed with the Schema the router encodes it with.
- A non-2xx answer throws `OkayHttpError`.

The end-to-end test runs a real okay-http server and a Node program
through the generated client. `tsc --strict` accepts the program and
refuses a wrong field. A shape test runs in the default gate, and a
mutant is caught. Docs: "A Scala backend, a TypeScript frontend" in
docs/typescript.md.
