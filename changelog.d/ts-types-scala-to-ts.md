## ts-types-scala-to-ts - the TypeScript (and Python) declarations written as a build step

T2 of specs/typescript-types.md. `okay.codec.StubFiles.typescript(file,
schemas*)` and `StubFiles.python(...)` write the generated declarations
into another codebase's tree. The file is rewritten only when its text
changed, so an unchanged model does not wake a frontend's watcher. With
a small `main` listing the types and an sbt task (shown in
docs/typescript.md), the types are written once in Scala and regenerated
by every build. Tests: 2, and a mutant is caught.
