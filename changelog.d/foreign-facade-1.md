## foreign-facade-1 - Calls[M] and Speaks[M]: tier 1 of the facade, one conformance body over Python, R and the JVM

Stage 1 of specs/foreign-facade.md. `Calls[-M]` is one typed call —
a `Schema` value in, one out, one line of the wire — with instances for
`PyModule`, `RModule` and `JvmModule` (`JvmModule.fn[A, B](name)(f)`
registers a function for the zero-cost tier); `Speaks[-M]` reports what
this worker DOES (link, how a frame crosses, programs). A capability a
module type lacks is a compile error at the call. `FacadeConformance`
is one body per capability, run by `TestFacade` over the JVM and a
test's own module type in the default gate, and by `TestPyFacade` /
`TestRFacade` (Live) over real interpreters — the same text. Python
3.14 on this box is green; R is not installed here and its suite skips.
Refusals are one type on every road (`Batcher.Failed`); a missing JVM
function is refused by kind at call time, as Python's AttributeError
is. Results in the spec say what stage 1 chose and why.
