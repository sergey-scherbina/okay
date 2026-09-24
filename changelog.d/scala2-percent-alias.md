## scala2-percent-alias - `State % Int` in Scala 2: `type %[F[_], A] = F[A]`

- Operator asked for okay's application combinator. An alias that TAKES
  a type constructor and answers a plain type is fine in 2.13 (what it
  refuses, stage 16, is one that ANSWERS a constructor), so `State % Int`
  is `State[Int]`, `=:=` both ways; `Int ! (State % Int)` runs.
- In a row it needs its own parentheses: with one precedence for every
  infix type operator (stage 17), `State % Int + Writer % String` is
  `((State % Int) + Writer) % String`, refused as "`State % Int +
  Writer` takes no type parameters, expected: 1" — pinned with
  `compileErrors` in `TestRowAliasFromScala2`. `(State % Int) + (Writer
  % String)` is longer than `State[Int] + Writer[String]`, so the probe
  and the docs keep the bracket form; docs/scala2.md section 3 says so.
- Declared beside `+` and `!` in the probe's package object; spec stage 18.
