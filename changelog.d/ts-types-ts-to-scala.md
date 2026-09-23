## ts-types-ts-to-scala - TypeScript declarations read into Scala types; the round trip exact

T3 of specs/typescript-types.md. `okay.codec.TsTypes.scala(source, pkg)`
reads the data subset of TypeScript declarations and writes the Scala
whose Schemas make okay's JSON write what those types describe:
- an interface becomes a case class;
- a union of `{ Case: Case }` becomes an enum;
- `T[]` becomes `Vector`, and `T | null` becomes `Option`;
- an optional field becomes `Option = None`.

Generics, intersections, methods, maps and literal unions are refused
by name and line. It is pure Scala, so it runs on every platform
without Node.

`Stubs.typescript` now names the Scala leaves TypeScript would collapse
(`Int`, `Long`, `Char`, `BigIntDigits`, `Base64`), so Scala → TypeScript
→ Scala is exact. A test checks it through a golden file that compiles
and whose TypeScript equals the original byte for byte. Tests: 5, on all
platforms, and every live `tsc` check still passes. A mutant is caught.
Docs: "Types written in TypeScript first" in docs/typescript.md (pinned),
and docs/modules/okay-codec.md.
