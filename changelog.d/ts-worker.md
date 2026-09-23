## ts-worker - TypeScript on the okay wire

Stage 1 of specs/typescript.md. The jar ships `/okay/ts/okay.ts`, the
library a module imports, and `/okay/ts/worker.ts`, the process.
`TsWorker.start(dir, modules)` runs `node worker.ts` on the same wire as
the Python shim. Node strips the types, so there is no build step. The
okay-py API drives it unchanged:

- typed calls (`Py.fn`);
- callbacks into okay's effects, through `call("name", x)` in TypeScript;
- held objects (`Py.hold`);
- programs as data (`done`/`perform`/`then`), MULTI-SHOT under `Choice`;
- async functions, awaited;
- `Durable`.

`Stubs.typescriptWire` declares the shapes the wire hands TypeScript. A
sum carries `type: "Case"`, and a Long is `number | bigint`.

Tests: 7 live, including `tsc --strict` on a module typed with the
generated declarations, which also refuses a wrong field. One shape test
runs in the default gate, and a mutant is caught. Docs: new page
docs/typescript.md (pinned, with a diagram), linked from the README and
docs/README.md. backlog polyglot-typescript now lists stages 2–5.
