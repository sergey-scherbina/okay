## scala2-bang-alias - `A ! R` in Scala 2: `Int ! (State[Int] + Writer[String])`

- Operator asked for `A ! F = Eff[F, A]` beside `+`. The alias is
  `type ![A, R] = Eff[R, A]`, declared next to `+` in the user's
  package object (the probe's `package object scala2probe`), so a
  Scala 2 program type reads as okay's:
  `Int ! (State[Int] + Writer[String])`.
- The parentheses are REQUIRED, and a belief that they were not cost
  an hour: Scala 2 gives every infix type operator one precedence,
  left-associative (SLS 2.13 §3.2.8; the first-character rule is Scala
  3's), so `Int ! Choose + Writer[String]` is `(Int ! Choose) +
  Writer[String]` — scalac printed `Eff[Choose, Int] with
  Writer[String]`. `TestRowAliasFromScala2` pins `=:=` both ways with
  `Eff[R, A]`, the chained row, the left association itself, and
  contravariance kept.
- Every `Eff[R, A]` type in the probe (56) and in the Scala 2 docs
  (guide, module pages, typepedia, guide.md, theory ch. 13,
  jvm-languages, your-own-effect; ~200) is now `A ! R`. What stays:
  the `Eff[-R, A]` declaration, library signatures quoted with `&`, the
  compiler message `State[Int] with Any`, and `Eff[Async, _]` in prose.
- Spec stage 17.
