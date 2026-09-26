## foreign-one-held — one call for a function, a method, an attribute, a held answer (2026-09-26)

Stage 2c of specs/foreign-one.md. `hold`, `method` and `attr` were calls
with a particular address or answer; now they are one:
`call{fn: address, args, held}` on the wire, and on the host
`ForeignEval.Call(fn: Address, args, held)` with `into enum Address`
(`Fn(name)`, `Method(ref, name)`, `Attr(ref, name)`; a `String` IS a
function's name, so `REval.Call("stats::median", args)` reads unchanged).
`ForeignEval.Hold`/`Method`/`Attr` are gone; the effect has six cases.
Python and TypeScript speak shim 8, R shim 11 (a method or attribute
address is refused by name: R applies functions to objects), and the Go,
Rust and Haskell libraries share version 8. The pool, the supervisor, the
journal (names unchanged) and the cluster's held models and stateful
stages follow. Live green on the first run: okay-py 226, okay-r 98,
okay-rust 35, cluster 30, workflow 12. Mutant: the pool not registering a
held answer fails both pool-of-handles tests.
