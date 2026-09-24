## okay2-distinct-handlers - every okay2 handler of a parameterised signature refuses a row holding two of its class

- Operator: guard the per-signature handlers too (and the Scala 3 core
  the same way: backlog `distinct-on-handlers`). Measured first: 
  `State.handle(1)` over `State[Int] + State[String]`, `Reader.run` over
  two Readers and `Writer.run` over two Writers all compiled.
- `Distinct[Sig with R]` is now required by every public eliminator that
  splits a PARAMETERISED signature out of an open row: State, Reader,
  Writer, Throws (with the `…At` twins forwarding it, and
  `recover`/`orElse`), the kernels `relay`/`translate`/`interpret`/
  `handle`, `toFs2`/`toZStream`. `interpret` now checks its target row
  too. Unparameterised signatures (Delim, Once, Resource, Choose,
  Async) cannot occur twice with different types and carry no check.
- okay2's own main code never summons the macro (it cannot expand in
  the run that defines it): it forwards the caller's evidence, or passes
  `Distinct.unchecked` where the split is of an unparameterised
  signature, with the reason beside it.
- 329 okay2 tests green cold; no existing test changed. Spec stage 10,
  docs/okay2.md section 3.
