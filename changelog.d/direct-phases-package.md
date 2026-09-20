## direct-phases-package - the direct compiler's phases move to okay.macros

The nine `Direct*` phase files of direct-compiler-phases sat flat in
`package okay` beside the facade — machinery every file of the core
could see. They live in `src/main/scala/macros/` now, `package
okay.macros` (5c86f01c; spec and board 65a31144): `okay.Direct` is
unchanged, its one call is `macros.DirectCompiler.pipeline`, and the
test-side probes import the class.

The obvious name, `okay.direct`, was refused by a probe compile
rather than by argument: of the three import shapes the repository
uses, `package okay` + `import Direct.*` and a bare `import
okay.Direct.*` both compiled, and `import okay.*` beside `import
okay.Direct.*` — seven files here, and any user's natural spelling —
failed with E049 "Reference to direct is ambiguous: imported by
okay._ and by okay.Direct._". A package is a term name too. The
Decisions entry that had called this "shadowing" is corrected.
