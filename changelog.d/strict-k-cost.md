## strict-k-cost — a machine alone answers its value itself again

Operator, 2026-10-04: "делай как предлагаешь" (of three ways to take back
statePara's 1.12x against cont-atm).

- The allocation profile (async-profiler, `-agentpath`, event=alloc): one
  `Freer.Return` more a strict `k` than cont-atm — the one loop answers
  `Free[F, Z]`, so a forced `k` built a `Return` to take apart.
- `Run.goAlone`: `go`'s arms less the two a machine alone cannot reach (an
  operation sent out, a deferred run stepped into), answering `Z`.
  `Machine.run`/`value` and a strict `k` on a machine alone go through it. No
  cast in the machine still.
- statePara 0.94x, fib100 0.95x, contAnswer 1.00x master's (history.d
  strict-k-cost); against cont-atm 1.06x left.
- Backlog: nine items on what is still too complex or slow in
  Freer/Delimited/Cont/Effects (handler-one-step first).
