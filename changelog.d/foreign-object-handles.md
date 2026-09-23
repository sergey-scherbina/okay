## foreign-object-handles - Python and R objects held behind a handle

Stage 3 of specs/foreign-highlevel.md. `Py.hold(fn)(args)` and
`R.hold(fn)(args)` keep a result on the far side and answer a `PyRef` or
`RRef`. A ref is a value on the wire, so it can be passed to any
function (`stats::predict(fit, newdata)`). A Python ref also has its
methods (`ref.call`, `ref.hold`) and attributes (`ref.attr`). Release is
idempotent, and a ref that has been released or belongs to another
process is refused by name. Arguments go through `ToPy`/`ToR`, so
`Schema` values and refs mix.

`PyWorkers` sends a ref's calls to the worker that holds it, and that
worker stays in the pool. Pulling it out, the first design, deadlocked a
pool of one. A whole program with handles replays from the journal;
recovery onto a fresh process is refused by name. The shims move to
Python 4 and R 5.

Tests: 8 live (a seeded Random, `lm` plus `predict`, both pools, Durable
replay versus recovery) and 1 journal test in the default gate. A
mutant is caught. Docs: "Held objects" in docs/modules/okay-py.md and
docs/modules/okay-r.md. backlog `polyglot-remote-foreign` now says which
half of it foreign-callbacks built.
