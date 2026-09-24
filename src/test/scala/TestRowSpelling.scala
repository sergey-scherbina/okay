import okay.*

/**
 * The spelling of a program's type, pinned (bang-row-no-parens,
 * 2026-09-24). Effects.scala:87 says why: an infix type operator's
 * precedence comes from its first character, `!` sits at the `= !`
 * level, `+` and `%` above it — so `A ! State % S + Writer % W` needs
 * no parentheses, and every page and test writes it bare. (Scala 2
 * gives every infix type ONE precedence; the facade's rows keep their
 * parentheses, `TestRowAliasFromScala2` pins that side.)
 */
class TestRowSpelling extends munit.FunSuite:
  test("A ! F % S + G % W is A ! ((F % S) + (G % W)): + and % bind before !"):
    summon[(Int ! State % String + Writer % String) =:= (Int ! (State % String + Writer % String))]
    summon[(Int ! State % String + Writer % String) =:= Free[State % String + Writer % String, Int]]
    summon[(Unit ! Writer % Int + Async) =:= (Unit ! (Writer % Int + Async))]

  test("a program written at the bare row runs"):
    import okay.RowLift.at
    val prog: Int ! State % String + Writer % String =
      for
        s <- State.get[String].at
        _ <- Writer.tell("saw " + s).at
      yield s.length
    assertEquals(!.run(Writer.collect(State.handle("abc")(prog))), (Vector("saw abc"), ("abc", 3)))
