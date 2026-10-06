## okay-cont: the effect library of the machine

Feature, lane cont-effects (specs/freer-min.md, stage 35). `State`,
`Reader`, `Writer`, `Throws`, `Choose`, `Emit` (`collect` in place,
`generate` lazy) and `Dialogue` (`question`, `Paused`, `drive`) as handlers
of the machine — delimiters, with doors as fragments (`get`, `put`,
`modify`, `ask`, `asks`, `tell`, `raise`, `among`, `yield_`, `question`).
The kernel: a `Handling` context carries the handler's CLAUSE at its own
level (made once at the install, what an `Op` carries), and `handle` has a
clause form, `handle(ret)(clause)(body)`, for a handler that keeps the
continuation as a value at a known level. Measured against the classic
handlers: `writerTell` 18.5 vs 27.7 µs, `stateAnswering` 21.9 vs 18.8.
