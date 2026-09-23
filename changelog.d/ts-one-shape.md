## ts-one-shape - one set of types for Scala and TypeScript, in all three places they meet

T1 of the new specs/typescript-types.md. The spec and the board now
plan the operator's "the same types, not written twice" across the three
scenarios:
- a Scala backend with a TypeScript frontend;
- both in the browser;
- both on the backend.

The plan: one shape, one source, one check (T1–T7 in the sprint queue,
frontend ideas in the backlog).

This lane delivers the one shape. okay-py's call classes now take a
`Shape`, and the new `Ts` API is `Py` with okay's JSON codec as its
shape. A TypeScript worker therefore receives exactly what an okay HTTP
endpoint sends and what okay-ts hands a browser program. So ONE
`Stubs.typescript` declaration file, generated from the Scala types,
types a TypeScript module in all three places.

Tests: 4 live, including a literal comparison with the HTTP JSON and
`tsc --strict`. A mutant is caught. Every existing okay-py test still
passes. Docs: "One set of types, three ways to run" in
docs/typescript.md.
