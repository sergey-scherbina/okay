- [ ] fuse-plan-typed-term — `Fuse.scala`'s `Plan.L(get: Any, put:
      Any)` (~line 70) holds macro terms as `Any`, and the ten
      `asInstanceOf[Term]` at ~166-229 all read that one pair back.
      That is the shape the operator's rule names outright: an `Any`
      where a type parameter would do (AGENTS.md, "no cast without a
      real necessity"). The reason it is `Any` is real but not
      binding — `Plan` is declared outside any `Quotes`, so it cannot
      name the path-dependent `q.reflect.Term` — and the typed route
      is the ordinary one: `enum Plan[T]` with `L(get: T, put: T)` and
      `Then(outer: Plan[T], inner: Plan[T])`, instantiated at
      `q.reflect.Term` inside `plan`/the emitters, so every read is a
      field access. Mechanical; the gate is `okay-optics` compiling
      warning-free plus its existing fusion parity tests
      (optics-fuse reaches the hand-written update byte for byte —
      that law is the check nothing moved). Not in `optics-outside`'s
      API question, filed here because it is the open optics section.
      Found in the 2026-09-20 review.
