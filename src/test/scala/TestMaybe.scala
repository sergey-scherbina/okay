package okay

import okay.Row.{at, plus}

/** specs/core-gaps.md, Maybe: an absence of its own, not a Throws */
class TestMaybe extends munit.FunSuite {
  val ages = Map("ann" -> 31, "bob" -> 42)
  def age(name: String): Int ! Maybe = ages.get(name).maybe

  test("Some answers, None stops, run answers the Option") {
    assertEquals(!.run(Maybe.run(age("ann"))), Some(31))
    assertEquals(!.run(Maybe.run(age("eve"))), None)
    val both = for a <- age("ann"); b <- age("bob") yield a + b
    assertEquals(!.run(Maybe.run(both)), Some(73))
    val stops = for a <- age("ann"); b <- age("eve") yield a + b
    assertEquals(!.run(Maybe.run(stops)), None)
  }

  test("a row holds Maybe AND Throws, and each handler answers its own") {
    type R = Maybe + Throws % String
    def adult(name: String): Int ! R =
      age(name).at[R].flatMap(a => if a >= 18 then pure(a) else raise[String, Int]("minor").at[R])
    def both(n: String) = !.run(runEither(Maybe.run(adult(n))))
    assertEquals(both("ann"), Right(Some(31)))
    assertEquals(both("eve"), Right(None))
    val young: Int ! R = pure[R, Int](7).flatMap(a => if a >= 18 then pure(a) else raise[String, Int]("minor").at[R])
    assertEquals(!.run(runEither(Maybe.run(young))), Left("minor"))
    summon[Distinct[Maybe + Throws % String]]
    // the review's point 1: Abort IS a Throws, so this row is refused
    assert(compileErrors("summon[Distinct[Abort + Throws % String]]").nonEmpty)
  }

  test("effects after a Some run, effects after a None do not") {
    def logged(name: String): Int ! Maybe + Writer % String =
      for
        _ <- Writer.tell(s"looking up $name").at[Maybe + Writer % String]
        a <- age(name).plus[Writer % String]
        _ <- Writer.tell(s"found $a").at[Maybe + Writer % String]
      yield a
    assertEquals(!.run(Writer.run[String, Option[Int], Pure](Maybe.run(logged("ann")))), (List("looking up ann", "found 31"), Some(31)))
    assertEquals(!.run(Writer.run[String, Option[Int], Pure](Maybe.run(logged("eve")))), (List("looking up eve"), None))
  }

  test("orElse tries the alternative only on None, getOrElse answers a default") {
    assertEquals(!.run(Maybe.run(age("eve").orElse(age("bob")))), Some(42))
    assertEquals(!.run(Maybe.run(age("ann").orElse(age("bob")))), Some(31))
    assertEquals(!.run(age("eve").getOrElse(0)), 0)
    assertEquals(!.run(age("ann").getOrElse(0)), 31)
  }

  test("a refutable pattern in a Maybe row stops through Maybe") {
    val pairs: Option[Int] ! Maybe = pure(Some(1))
    val nones: Option[Int] ! Maybe = pure(None)
    def first(p: Option[Int] ! Maybe): Int ! Maybe = for case Some(x) <- p yield x
    assertEquals(!.run(Maybe.run(first(pairs))), Some(1))
    assertEquals(!.run(Maybe.run(first(nones))), None)
    assertEquals(!.run(Maybe.run(Maybe.none[Int])), None)
  }

  test("collect skips the elements that are not there, and goes on") {
    assertEquals(!.run(Maybe.collect(List("ann", "eve", "bob"))(age)), Vector(31, 42))
  }

  test("prune: under Choose a branch that found nothing dies, the others go on") {
    val known: Int ! Maybe + Choose = for n <- choose("ann", "eve", "bob").plus[Maybe]; a <- age(n).plus[Choose] yield a
    assertEquals(!.run(runChoice(Maybe.prune(known))), Seq(31, 42))
  }

  test("stack-safe over 100 000 binds") {
    val n = 100000
    val p = (1 to n).foldLeft(pure[Maybe, Long](0L))((acc, i) => acc.flatMap(s => Some(i).maybe.map(s + _)))
    assertEquals(!.run(Maybe.run(p)), Some(n.toLong * (n + 1) / 2))
  }
}

/** specs/core-gaps.md, Either → Throws */
class TestOrRaise extends munit.FunSuite {
  test("Right answers, Left raises") {
    val ok: Either[String, Int] = Right(42)
    val bad: Either[String, Int] = Left("no")
    assertEquals(!.run(runEither(ok.orRaise)), Right(42))
    assertEquals(!.run(runEither(bad.orRaise)), Left("no"))
    val sum = for a <- ok.orRaise; b <- bad.orRaise yield a + b
    assertEquals(!.run(runEither(sum)), Left("no"))
  }
}
