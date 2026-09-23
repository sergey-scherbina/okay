## ts-typed-effects - a TypeScript program's effects, in its type

T12 of specs/typescript-types.md. A TypeScript program used to name its
operations by strings. Now its operations can be part of its type, as a
record of signatures:
- `Prog<T, Ops>` and `effects<Ops>()` (`perform`, `call`, `andThen`,
  `done`) are in okay's worker library and in `@okay/ts`;
- `@okay/ts`'s `run` and `durable` take `Handlers<Ops>`, one per
  operation.

tsc refuses an unknown operation, a wrong argument, and a run with a
missing handler. `Ts.ops` (okay-py, okay-ts) and `Stubs.typescriptOps`
write the Ops from the Scala callbacks' Schemas, so the row is written
once, in Scala. Untyped programs still compile.

Checked live in the worker and through the npm package, and a mutant is
caught.

Docs: "A program's effects, in its type" in docs/typescript.md, and
"Where each language names okay's effects" in docs/jvm-languages.md.
