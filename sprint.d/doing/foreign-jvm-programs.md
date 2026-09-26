- [ ] foreign-jvm-programs — Clojure and Frege programs as data through the
      facade's `Programs` (specs/foreign-one.md Decision 8: a Clojure
      `(step op k)` or a Frege `Step op k` is walkable by `okay.Foreign`,
      so foreign-facade's "no JVM Programs" narrows to the Scala-function
      module). A `CljModule`/`FregeModule` with a `Programs` instance whose
      `run` is the walker. Trigger: a facade caller with a Clojure or Frege
      program; until then okay-clojure/okay-frege serve them directly.
