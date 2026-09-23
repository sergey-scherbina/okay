## ts-facade - a TypeScript module as a generated, typed Scala facade

T7 of specs/typescript-types.md: Scala and TypeScript both on the
backend. This is the TypeScript twin of PyFacade.

- `TsFacade.declarations(dir, module)` gets the module's types from tsc
  (`--emitDeclarationOnly`), so types the source left to inference are
  exact.
- `TsFacade.render` writes the module's data types as case classes and
  enums, and one method per exported function calling it through
  `Ts.fn`:
  - `Promise<T>` answers `T`;
  - open types become type parameters;
  - optional parameters are left out;
  - what it cannot type becomes a comment saying why.
- `TsTypes.parseModule` reads exported functions and imported type
  names.

A checked-in golden facade is regenerated and compared in a live test,
and its methods call the real worker. A mutant is caught.

Docs: "A typed Scala facade for a TypeScript module" in
docs/typescript.md.
