## foreign-one-program — one program protocol on the wire, in all six far sides (2026-09-26)

Stage 2b of specs/foreign-one.md. The direct style (`start`, `ask`,
`resume`) and programs as data (`program`, `perform`, `continue`) were two
protocols carrying the same three messages. Now there is one: a direct
function IS a program, its `okay_call` a `perform` node marked `once` (a
parked stack, continued at most once), continued by the same `continue`
that continues a program as data. Python, TypeScript (shim 7), R (shim
10), and the Go, Rust and Haskell libraries (7) speak it; Go and Rust also
gain a plain `call`.

Host: `ForeignEval.Start`/`Resume` and `PyStep` are gone; `Program`
carries the offered callbacks and the host's `direct` intent, `Continue`
an answer or a failure. The API's `Fn.calling`, `PyRun`, the pool, the
supervisor and the workflow activity walk one node type. The pool holds a
worker only while a `once` node is open; the supervisor never replays a
`once` continuation (WorkerDied as data) and never re-runs a direct start.

Live: okay-py 226, okay-r 98 (+6), okay-rust FFM/wasm 35, cluster 26,
workflow 12; tests changed only in constructors, the journal's op names,
and one case — a function returning a plain value is now a finished
program, not a refusal. Mutant: the supervisor ignoring `once` re-ran a
killed direct function and answered, silently — two suites catch it.
Journals holding a pre-lane direct dialogue are refused by the drift check.
