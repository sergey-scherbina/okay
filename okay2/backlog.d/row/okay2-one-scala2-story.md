- [ ] okay2-one-scala2-story — a question for the operator, raised by the
      review (2026-09-24): the repository now has TWO Scala 2 roads,
      the facade `okay-scala2` (wraps the Scala 3 library, 17 modules:
      http, sql, llm, ui, …) and okay2 (the core re-implemented, no
      Scala 3 on the classpath). Their SURFACE already agrees — both
      write `Int ! (State[Int] + Writer[String])` — but the `+` means
      different things: in the facade it is the user's alias for
      `with` (a contravariant intersection), in okay2 a sealed union
      trait that does not commute. If `okay2-intersection-row` is
      taken, the two become one encoding, and the facade's user code
      would compile against okay2 unchanged except for imports. Decide
      which road a new Scala 2 user is sent down, and whether the
      facade's modules are ever re-based on okay2.
