## foreign-one-mux — far-side sources, derived; duplex multiplexing deferred (2026-09-26)

Stage 5 of specs/foreign-one.md, narrowed by Decision 17. A far-side
stream — a generator, a cursor, a file read on the far side — needs no new
operation: `PyStream.pulled` holds the iterator (`call … held`), asks for one
chunk per call only when the consumer has taken the last, and releases it at
the end or on a failure. `Py.source[A]("m:rows")(args)` reads a Python
generator (`StopIteration` its end); `R.source` an R closure (NULL its end).
Live: `TestPySource` (order; BACK-PRESSURE — a consumer taking four of ten
asks for two chunks of three; a failure by name) and `TestRSource`. Mutant:
reading ahead fails both the order and the back-pressure tests. Docs: the
toolkit table and an example in docs/python-and-r.md. Filed:
foreign-mux-duplex (credits and several requests in flight, for its first
caller); stateful-early-stop now names sources.
