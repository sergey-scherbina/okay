## polyglot-roadmap - the next language bridges, filed with what is possible

A backlog lane, no code. It adds a new `polyglot` section and nine
items, answering the operator's question of which other languages okay
should talk to, and how:

- TypeScript (not supported today, and the item says so) in five
  ordered roads.
- Haskell: Frege already covers the language. A GHC subprocess is the
  one foreign runtime where multi-shot survives a process boundary, and
  in-process GHC is refused.
- Rust as compute: Scala Native `@extern`, the FFM API, or Wasm through
  Chicory.
- Go: network or Wasm only. An in-process c-shared library is refused
  because it would be a second GC and scheduler in the JVM.
- The shared pieces: `Foreign` over a wire, and types generated from a
  `Schema` for the other side.
- Under okay-py: GraalPy as an in-process engine, and Jython as its
  own module. Jython is Python 2 only, which was checked on 2026-09-23.
- A question to test: whether cancelling a fiber interrupts a lifted
  Frege or Clojure step.
- Under okay-scala2: facades for the language bridges, which none of
  the stage-15 lanes covered.
