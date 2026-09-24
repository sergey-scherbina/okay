- [ ] okay2-one-scala2-story — a question for the operator, raised by the
      review (2026-09-24): the repository now has TWO Scala 2 roads,
      the facade `okay-scala2` (wraps the Scala 3 library, 17 modules:
      http, sql, llm, ui, …) and okay2 (the core re-implemented, no
      Scala 3 on the classpath). Their SURFACE already agrees — both
      write `Int ! (State[Int] + Writer[String])` — but the `+` means
      different things: in the facade it is the user's alias for
      `with` (a contravariant intersection), in okay2 it WAS a sealed
      union trait that did not commute. Since okay2-intersection-row
      (2026-09-24) both are the same encoding, `with` under a
      contravariant program type, so the facade's user code is close to
      compiling against okay2 (differences left: `Eff` vs `Free`, the
      facade's `Handler` shape, imports). Decide
      which road a new Scala 2 user is sent down, and whether the
      facade's modules are ever re-based on okay2.
