## polyglot-schema-stubs - Python and TypeScript declarations generated from Schema

`okay.codec.Stubs.python(schemas*)` writes a module of `TypedDict`s in
the shape okay-py sends, with sums as unions discriminated on `"type"`.
`Stubs.typescript(schemas*)` writes a `.d.ts` in the shape okay's JSON
writes. The other side's type checker then sees the type okay checks at
the boundary. Recursive types and dependency order are handled.

Checked by the real checkers (Live): `tsc --strict` and
`uvx mypy --strict` accept what the codecs actually SEND, and refuse a
read of a field that does not exist. The mypy check found a defect on
its first run: a BigInt past a Long crossed to Python as a `str`. The
codec was fixed (`PyValue.BigI`), not the stub.

Tests: 4 in the default gate and 2 live. Docs: "Types on the other
side" in docs/python-and-r.md, "The other side's types: Stubs" in
docs/modules/okay-codec.md; spec: specs/schema-stubs.md.
