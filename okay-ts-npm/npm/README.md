# @okay/ts

okay for TypeScript projects: the Scala library [okay](https://github.com/sergey-scherbina/okay),
compiled with Scala.js into an ES module, with its types in `index.d.ts`.

- `run(program, callbacks)`: a program written as data with `done`, `perform`
  (or `performing`) and `then`, walked by okay. Each operation the program
  performs is one of your callbacks, which may answer a value or a Promise.
- `gcounter`, `pncounter`, `orset`: CRDT replicas as plain JSON states, each
  with `merge`. Replicas updated apart merge to the same state in any order.
- `channel()`: an okay channel you `offer` to, `close`, and read with
  `for await`.

`index.d.ts` is written by the module itself, from the same Scala schemas
that encode its values.
