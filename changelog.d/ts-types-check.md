## ts-types-check - tsc decides whether two TypeScript copies are the same types

T4 of specs/typescript-types.md. `okay.codec.TsCheck.same(generated,
handwritten)` and `TsCheck.sameAs(file, schemas*)` ask `tsc` whether
every type in the generated declarations is mutually assignable with the
hand-written one. Field order and formatting do not matter. A renamed
field, a field made optional, or a missing type is reported by the
type's name, in time to fail a build. The generated file's leaf aliases
are compared through, not by name. Tests: 4 live against `tsc`, and a
mutant is caught. Docs: "Two copies, kept honest" in docs/typescript.md.
