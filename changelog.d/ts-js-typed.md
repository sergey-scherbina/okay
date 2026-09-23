## ts-js-typed - Scala code printed as TypeScript by okay-js

T8 of specs/typescript-types.md.

- `Direct.ts { }` reads the `js { }` subset and keeps the types the Scala
  compiler inferred. `Js.printTs` prints the result as TypeScript
  (`var n: number = 1;`, `function (x: number, y: string)`), and
  `Direct.tsSource` is the same text as a compile-time constant.
- `Dyn` and a spliced `Js` are `any`. Any other type outside the short
  list is refused by name.
- `tsc --strict` accepts the output, and a test shows the annotations
  bind.

Two defects of `js { }` found on the way, both producing JavaScript that
runs and is wrong:
- `f(1)` on a function val printed `f.apply(1)`;
- `d.f = v` on a Dyn printed `d.updateDynamic("f", v)`.

Both are fixed, with tests.

Docs: docs/modules/okay-js.md ("TypeScript: `Direct.ts { }`") and
docs/typescript.md ("Scala code as TypeScript").
