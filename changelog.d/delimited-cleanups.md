## delimited-cleanups — a Piece starts at its first segment; Resume-as-Pending refuted

Operator, 2026-10-04 ("Начинай", after the list of what to simplify further).

- `Delimited.Piece` has no `Nil`: it starts at `One(k, tag)`, so a capture
  allocates no empty piece. delimGenerator 0.99x — kept for the shape.
- REFUTED (history.d delimited-cleanups): `Resume` as its own `Pending` —
  40 KB fewer a run, yet delimGenerator 1.10x and delimDollarResume 1.05x.
- Checked, nothing to do: Cont's deferred force builds a fresh machine, but its
  `k(a)` answers a program without running it, so no depth accumulates.
- Not done, with reasons in specs/cont-atm.md: dropping `Shift.Cut`, a `Diag`
  arm without the `Inject`.
