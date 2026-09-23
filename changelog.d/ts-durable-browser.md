## ts-durable-browser - durable flows in the browser, journalled in IndexedDB

T10 of specs/typescript-types.md.

- `Ts.durable(flow, program, callbacks, journal)` in okay-ts, and
  `durable` in `@okay/ts`: a TypeScript program's answers are journalled
  before it continues.
  - A reload replays the flow; recorded steps are answered from the
    journal without calling their callbacks again.
  - A recorded step asked differently is refused as `Drift`.
  - Journals: `Journal.memory()` and `Journal.indexedDb(name)`, and any JS
    object with `load`/`append`/`clear`.
- Checked in real headless Chrome by
  `scripts/ts-durable-browser-check.sh`: the first load dies at the
  charge, and the second finishes with each callback called once.
  `scripts/chrome-read.mjs` drives Chrome through the DevTools protocol.
- A mutant without the replay is caught by the unit tests and by
  Chrome.

Docs: "Durable flows in the browser" in docs/typescript.md.
