## foreign-source-early-stop — a far-side source's iterator is given back however its consumer ends (2026-09-26)

A consumer that stopped reading a `Py.source` / `R.source` early left the
iterator held on the far side until the worker ended. Now a source performs
`Holding.Hold` when it takes the iterator (and `Holding.Let` when it
releases it itself), its row carries `Holding`, and `Py.releasing` /
`R.releasing` — the only handler for it — release whatever is still held
when the program ends, as an ordinary `ForeignEval.Release` through the
caller's handler (pool, supervisor and journal see it like any call). A
source outside the scope does not compile. TestPySource: four of 10 000
rows read, the iterator released once, and refused afterwards on the far
side. specs/foreign-one.md Decision 22; docs/python-and-r.md.
