- [ ] okay2-delim-stacked — `Delim.Stacked` for okay2: the prompt stack
      in the type, so a shift to a prompt that is not installed is a
      COMPILE error. The Scala 3 core carries the stack as a tuple type
      (`p.type *: S`) in a lexical given; Scala 2 has no `*:`, so the
      stack is an HList-shaped type (`Cons[P, S]`/`Nil`) with `Has`
      instances over it, and the evidence is a value passed as the
      other doors' is. Trigger: a program whose NoPrompt reached
      production. (2026-09-24)
