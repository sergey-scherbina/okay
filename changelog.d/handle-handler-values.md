## handle-handler-values - one handle, the handlers values

Level 1 of the API, step 3 (operator, 2026-10-02: "Да" to
`p.handle(State(5))`; specs/shift-effect.md).

- `p.handle(h)` takes an effect off the row for every ready effect, and
  the rest of the row is inferred. `p.run` runs a program with no effect
  left: `p.handle(State(5)).handle(Throws.either).run`, in either order.
- The handlers are values: `State(s)`, `Reader(r)`, `Writer.log`,
  `Throws.either`, `Choose.all`, `Maybe.option`, and `Reset[R]`, so
  `reset` is one of them. Their type is `Handling[E, I, O, Needs]`
  (`Handling.Plain` when the handler needs nothing of the rest of the
  row).
- The package-level `handle` extension of `A throws E` moved into `object
  throws`, its type's companion, because it took the name from every
  program.
- The old runners (`State.run`, `runEither`, …) stay. TestHandling has 4
  tests.
