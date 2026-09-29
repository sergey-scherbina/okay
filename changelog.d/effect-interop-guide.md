## effect-interop-guide — one guide for okay beside ZIO, cats-effect and Future

docs/effect-interop.md gathers what the interop lanes of 2026-09-28/29
built into one page: the map of every door in both directions (what it
waits by, what a cancellation on the far side does), ZIO in full
(`fromZIO`/`toZIO`/`toZIOAsync`, the whole `ZIO[R, E, A]` as
`ZioRow[R, E]`, `direct[Task]`, ZIO values in okay blocks), cats IO,
Future, blocking-or-callback, cancellation precisely, your own effect
(a Java `CompletableFuture` instance — tested in TestDirectForeign, with
its failure and cancellation), the pitfalls, and the literature. Every
Scala line is pinned by TestDocSnippets (control: an unpinned line turned
it red). Linked from docs/README.md, direct-style.md, okay-zio.md,
okay-cats.md.
