## scala2-row-alias - `+` for a Scala 2 row: `Eff[State[Int] + Writer[String], A]` instead of `with`

- Operator: an analogue of okay's `+[F[_], G[_]]` "so that in Scala 2
  it works everywhere instead of `with`". The alias is
  `type +[R, S] = R with S`, kind `*` on both sides, so it stands in
  every row the facade has: two built-ins, a user effect beside a
  built-in (`Effect[Console] + State[Int]`), and a chain
  (`Reader[Config] + State[Int] + Throws[String]`).
- It lives in Scala 2 code — the probe's `package object scala2probe`,
  and a user's own package object, one line — because a Scala 3
  top-level alias is invisible to scalac 2.13.
- Two shapes refuted on the way (spec stage 16): the core's
  `+[F[_], G[_]]` (an alias cannot answer a higher kind, and 2.13 does
  not simplify `C[A] with C[B]` to `C[A with B]`, so handlers break),
  and `+[R, G[_]] = R with Effect[G]` (its right side is kind `* -> *`,
  so nine of the probe's twelve rows, the built-in ones, stayed `with`).
- `TestRowAliasFromScala2` (3 tests, in the gate): `=:=` both ways for
  one `+` and a chain; a program at `Effect[Console] + State[Int] +
  Writer[String]` handled effect by effect in either order with no
  annotation; a one-capability program in a wider `+` row.
- Every Scala 2 row in the probe and in docs/scala2.md,
  docs/modules/okay-scala2*.md, typepedia, guide, theory ch. 13,
  jvm-languages and your-own-effect now reads `+`; section 3 of
  docs/scala2.md declares it.
