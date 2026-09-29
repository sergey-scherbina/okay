## direct-foreign-mark — a Future, a ZIO or a cats IO marked inside a direct block

A `direct` block over an okay program now binds another library's effect
with the same marks: `Future(20).?`, `ZIO.attempt(22).?`, `IO(1).reflect`,
`!io`. The macro's new case asks for a `ForeignEffect[M]` (okay-async; no
foreign dependency) and widens its `lift(m): A ! G` into the block's row
by subtyping, refusing at compile time when `G` is not in the row.
Instances: `Future` (in the companion, an Await on completion), ZIO in
three specificities (`Async`; `Throws % E + Async`; `ZioRow[R, E]`,
`import okay.zio.given`), cats `IO` (`import okay.cats.given` + an
`IORuntime`). `!z` stays refuted on ZIO (its own `unary_!`).

Found: a higher-kinded wildcard in `Implicits.search` finds nothing, so
the effect is a type member `G[+X]`, not a parameter.

Spec: specs/direct-foreign-mark.md. Docs: docs/direct-style.md ("Foreign
effects"), docs/modules/okay-zio.md, okay-cats.md, okay-direct.md.
