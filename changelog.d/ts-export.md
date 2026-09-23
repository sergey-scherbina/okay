## ts-export - okay functions on Scala.js for TypeScript callers, with their declaration

T6 of specs/typescript-types.md: Scala and TypeScript both in the
browser.

- `Ts.expose[A, B](name)(f: A => B ! Async)` is one function a TypeScript
  caller awaits. A wrong argument rejects the promise with a named
  `TypeError`.
- `Ts.module(name)(...)` gives two things: `.js`, to export with
  `@JSExportTopLevel`; and `.declaration`, a `.d.ts` with the codec's
  types and `export declare const name: { f(input: A): Promise<B> }`.
- `tsc --strict` accepts a caller written against that declaration and
  refuses a wrong field. A mutant with a wrong answer type is caught.

Docs: "A module of functions, with its declaration" in
docs/typescript.md.
