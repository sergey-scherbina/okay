- [ ] delimited-trait — PRIORITY: HIGH, operator ask 2026-10-01: the
      continuation machine behind an interface, Dybvig, Peyton Jones &
      Sabry's MonadDelimitedCont in our variant — trait `Delimited[M]`
      with four primitives (a fresh delimiter, `$`, `shift0` whose k
      keeps the delimiter and its `ret`, and resuming k with a
      COMPUTATION), the derived operators (reset, shift, abort) over it,
      the frame machine its instance, and the clients (Delim, Lexical,
      Cont, tests) written against it. Spec: specs/delimited.md.
