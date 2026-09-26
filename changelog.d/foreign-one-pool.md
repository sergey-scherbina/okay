## foreign-one-pool — one pool for every foreign worker (2026-09-26)

Stage 3 of specs/foreign-one.md. The cluster's `Pool`, `PyWorkers`' queue
and the facade's handle pools were three; now `okay.py.Pool` (moved from the
cluster, FIFO over idle interpreters) is the pool and `PyWorkers` its
routing layer (by ref, by run; a `once` program keeps its lease). The
cluster's `PyPool` and `RPool` are `PyWorkers` over it — R's workers are
`ForeignWorker`s since foreign-one-value — so stages, reduces, models,
stateful stages and the facade's handles, methods and programs share one
pool per (language, interpreter, module); the facade's second Python pool
and R's holder-of-one are gone.

Fixed: a stateful stage whose step failed kept its leased interpreter for
good (found on feature/foreign-streams-holds). `Streamer.abandon` gives the
state back on every failure path; `TestStatefulLease` was red before the
fix. Still open, filed as stateful-early-stop: a downstream that stops
pulling early. Live: okay-py 227, okay-r 98, cluster 30, workflow 12.
