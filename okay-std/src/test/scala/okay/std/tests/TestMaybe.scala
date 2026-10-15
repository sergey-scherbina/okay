package okay.std.tests

import okay.*
import okay.std.*

/** specs/core-gaps.md, Maybe: an absence of its own, not a Throws */
class TestMaybe extends munit.FunSuite {
  val ages = Map("ann" -> 31, "bob" -> 42)
  def age(name: String): Int ! Maybe = ages.get(name).maybe

  test("Some answers, None stops, run answers the Option") {
    assertEquals(Maybe.run(age("ann")).run, Some(31))
    assertEquals(Maybe.run(age("eve")).run, None)
    val both = for a <- age("ann"); b <- age("bob") yield a + b
    assertEquals(Maybe.run(both).run, Some(73))
    val stops = for a <- age("ann"); b <- age("eve") yield a + b
    assertEquals(Maybe.run(stops).run, None)
  }

  test("a row holds Maybe AND Throws, and each handler answers its own") {
    type R = Maybe + Throws % String
    def adult(name: String): Int ! R =
      age(name).at[R].flatMap(a => if a >= 18 then pure(a) else raise[String, Int]("minor").at[R])
    def both(n: String) = runEither(Maybe.run(adult(n))).run
    assertEquals(both("ann"), Right(Some(31)))
    assertEquals(both("eve"), Right(None))
    val young: Int ! R = pure[Int, R](7).flatMap(a => if a >= 18 then pure(a) else raise[String, Int]("minor").at[R])
    assertEquals(runEither(Maybe.run(young)).run, Left("minor"))
  }

  test("effects after a Some run, effects after a None do not") {
    def logged(name: String): Int ! Maybe + Writer % String =
      for
        _ <- Writer.tell(s"looking up $name").at[Maybe + Writer % String]
        a <- age(name).at[Maybe + Writer % String]
        _ <- Writer.tell(s"found $a").at[Maybe + Writer % String]
      yield a
    assertEquals((Writer.run[String, Option[Int], Writer % String](Maybe.run(logged("ann")))).run, (List("looking up ann", "found 31"), Some(31)))
    assertEquals((Writer.run[String, Option[Int], Writer % String](Maybe.run(logged("eve")))).run, (List("looking up eve"), None))
  }

  test("orElse tries the alternative only on None, getOrElse answers a default") {
    assertEquals(Maybe.run(age("eve").orElse(age("bob"))).run, Some(42))
    assertEquals(Maybe.run(age("ann").orElse(age("bob"))).run, Some(31))
    assertEquals(age("eve").getOrElse(0).run, 0)
    assertEquals(age("ann").getOrElse(0).run, 31)
  }

  test("a refutable pattern in a Maybe row stops through Maybe") {
    val pairs: Option[Int] ! Maybe = pure(Some(1))
    val nones: Option[Int] ! Maybe = pure(None)
    def first(p: Option[Int] ! Maybe): Int ! Maybe = for case Some(x) <- p yield x
    assertEquals(Maybe.run(first(pairs)).run, Some(1))
    assertEquals(Maybe.run(first(nones)).run, None)
    assertEquals(Maybe.run(Maybe.none[Int]).run, None)
  }

  test("collect skips the elements that are not there, and goes on") {
    assertEquals(Maybe.collect(List("ann", "eve", "bob"))(age).run, Vector(31, 42))
  }

  test("prune: under Choose a branch that found nothing dies, the others go on") {
    val known: Int ! Maybe + Choose = for n <- choose("ann", "eve", "bob").at[Maybe + Choose]; a <- age(n).at[Maybe + Choose] yield a
    assertEquals(runChoice(Maybe.prune(known)).run, Seq(31, 42))
  }

  test("stack-safe over 100 000 binds") {
    val n = 100000
    val p = (1 to n).foldLeft(pure[Long, Maybe](0L))((acc, i) => acc.flatMap(s => Some(i).maybe.map(s + _)))
    assertEquals(Maybe.run(p).run, Some(n.toLong * (n + 1) / 2))
  }
}

/** specs/core-gaps.md, Either → Throws */
class TestOrRaise extends munit.FunSuite {
  test("Right answers, Left raises") {
    val ok: Either[String, Int] = Right(42)
    val bad: Either[String, Int] = Left("no")
    assertEquals(runEither(ok.orRaise).run, Right(42))
    assertEquals(runEither(bad.orRaise).run, Left("no"))
    val sum = for a <- ok.orRaise; b <- bad.orRaise yield a + b
    assertEquals(runEither(sum).run, Left("no"))
  }
}
