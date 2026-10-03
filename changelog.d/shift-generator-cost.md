## shift-generator-cost — `shift` as one step, a one-segment continuation without a piece

After delimited-simplify (operator: "Потом можешь заняться оптимизацией").

- `shift` is ONE step of the machine: the capture and the fresh reset its body
  runs under, together — no `Push` operation, node and step a capture.
- A nested run stepped into with no barrier installs no boundary at all.
- A continuation of ONE segment (the usual Cont capture) is `Segment`: forced
  and resumed with no `Piece` and no `Next`; a machine alone asks no `Outer`.
- `Steps.apply` tests "an operation of another effect" first.
- Measured against master (history.d shift-generator-cost): delimGenerator
  0.73x (the λ$ machine's 54 us again), statePara 0.87x, fib100 0.92x,
  contAnswer 0.97x, stateForeign 1.01x. Against cont-atm what remains open
  is backlog delimited-simplify-costs.
