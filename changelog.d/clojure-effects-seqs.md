## clojure-effects-seqs - okay's effects from Clojure, and lazy seqs both ways

okay-clojure, second pass. `okay.core` is a Clojure namespace shipped as
a resource in the jar (`(require '[okay.core :as ok])`, no AOT). It is
okay's freer tree written in Clojure: `done`, `step`, `bind`, `perform`,
`await`, `tell`, and `mlet`, a monadic let in cats' shape.
`okay.clojure.Program` walks it as `stage`, `stageWith` (a stage that
also performs) or `run[F]`. Every operation runs under okay's handlers.
Every continuation is a Clojure function, so `Choose` resumes it per
branch, a hundred thousand steps run on the default stack, and no thread
is involved. `okay.clojure.Ops` provides the core effects' operations,
including `sleep` for Async. `Clj.value` reads a var's value.

`Program.chunks` and `Program.seq` convert between Clojure seqs and okay
`Chunks` lazily, infinite on either side and counted: five elements read
realise one Clojure block, and `(take 10 …)` over an infinite okay
source produces at most one chunk. `seq` accepts only pure `Chunks`.
Mutants: an eager bridge in either direction is caught, by bounded
tests. Docs: module page, guide §5.
