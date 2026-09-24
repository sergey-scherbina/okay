- [ ] scala2-percent-alias — `type %[F[_], A] = F[A]` beside `+` and `!`
      in the probe's package object (operator, 2026-09-24), so
      `State % Int` is `State[Int]` in Scala 2 as in okay. Lane: the
      alias, `=:=` both ways, the one-precedence parse of a chain
      pinned by a refusal (`State % Int + Writer % String` is a kind
      error in 2.13), docs §3 paragraph, spec stage 18. Rows stay
      `State[Int] + Writer[String]`: shorter than the parenthesised
      `(State % Int) + (Writer % String)` Scala 2 needs. (2026-09-24)
