## writerk-companion-scope - writerK found without an import, from any package

`writerK` (the `given TypeableK[Writer % W]` instance every `Writer.fold`/
`.collect`/`.run` call needs for a parameterized `W`) was a bare
top-level given in package `okay`, not in `object Writer`'s own
companion. That meant any file in a DIFFERENT package — every one of
`okay-blob`'s files this session, and every module producer-to-writer-
carrier's stage 2 will touch next — needed an explicit `import
okay.writerK` (or the wider `import okay.given`) just to satisfy that
`using` clause, or the compiler answered `E006 "Not Found"` naming
`summon`.

Moved `writerK` inside `object Writer { ... }`. Scala's implicit
search already looks at the companion object of every type mentioned
in a query, so `TypeableK[Writer % W]` now finds it from ANY package,
with no import — verified, not assumed: removed the explicit import
from `okay-blob`'s four files and confirmed they still compile.

Blast radius, checked properly: `import okay.given` (the wildcard that
pulls in every given in package `okay`, `writerK` included) appears in
over 300 files across the repo. A full `compile` and `Test/compile` of
every project, every platform, found exactly four files where `writerK`
was the ONLY reason that wildcard was there — `okay-llm/Llm.scala`,
`okay-llm/OpenAi.scala`, `okay-http/Acceptance.scala`, `okay-http/
Http.scala` — flagged `[E198] unused import` once the move made it
redundant. Removed there; every other file needed `okay.given` for
something else and is untouched.

Gate: `okayJVM/test` 743/743 (1 ignored, unrelated), `okayBlobJVM/test`
21/21, `okayLlmJVM/test` 20/20, `okayHttpJVM/test` 109/109 — all green.
Full-repo `compile` and `Test/compile`, every project and platform,
zero warnings.

The `@nowarn("msg=cannot be checked at runtime")` half of this pattern
(E092, the TypeableK caveat itself — the erasure check is genuinely
unverifiable, not a placement question) is unaffected and still needed
at each call site; this only removes the import half.
