## handle-values-rest - every ready effect has a handler value

Level 1 (operator's roadmap, 2026-10-02): `Once.memo`, `Resource.region`
(it needs the rest's `Failing`), `Fresh.counter`, `Supply.from(first)(step)`,
`Prob.exact`, `Chronicle.verdict` and `Async.blocking`, beside State, Reader,
Writer, Throws, Choose, Maybe and Reset. `p.handle(h)` now takes any ready
effect off. TestHandleRest.
