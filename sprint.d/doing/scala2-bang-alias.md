- [ ] scala2-bang-alias — `type ![A, R] = Eff[R, A]` beside stage 16's
      `+`, in Scala 2 code (the probe's package object; a user declares
      both), so a Scala 2 program reads `Int ! State[Int] + Writer[String]`
      as okay's `Int ! (State % Int + Writer % String)`. Scala 2 gives
      `!` lower precedence than `+`, so no parentheses. Lane: the alias,
      `=:=` both ways and a program annotated with it in
      TestRowAliasFromScala2, every `Eff[R, A]` type in the probe and
      the Scala 2 docs rewritten to `A ! R`, spec stage 17. (2026-09-24)
