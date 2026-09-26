## foreign-jvm-programs — Clojure and Frege programs through the facade (2026-09-26)

`Programs[CljModule]` and `Programs[FregeModule]`: a Clojure namespace's
function, or a registered Frege program, is walked in this process by
`okay.Foreign`, performing the same `Cb` callbacks every wire language's
program is offered — the facade's conformance body (a callback under a
Reader, multi-shot on every branch of two choices) passes over both, in the
default gate. A Frege list crosses to a callback as a Java array
(`JArray`), which `Jvm.frege` now types. okay-frege's Frege test classes
are exported to dependents. specs/foreign-facade.md, Decision 7.
