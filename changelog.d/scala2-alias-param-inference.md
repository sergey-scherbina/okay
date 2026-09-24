## scala2-alias-param-inference - the Scala 2 row aliases do not go in a parameter with an abstract row

- Measured: `def f[R, A](p: A ! (State[Int] + R))` compiles, and a
  call solves `R` as the WHOLE row, `State` included, so the handlers
  after it cannot finish the program (0 of 3 positions of `State` in a
  three-capability row); either alias alone does the same.
  `p: Eff[State[Int] with R, A]` infers `R` in all three. scalac 2 does
  not look through an alias to solve a row variable. Concrete rows and
  result types are unaffected, so every program type in the probe and
  the docs stays as stages 16-18 wrote it.
- Pinned both ways in `TestRowAliasFromScala2`; docs/scala2.md section 3
  gives the rule and the spelling; spec stage 19.
