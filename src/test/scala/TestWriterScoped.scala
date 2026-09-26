package okay

import okay.Row.at

/** specs/core-gaps.md stage 2: Writer.listen and Writer.censor */
class TestWriterScoped extends munit.FunSuite {
  type W = Writer % String
  def say(s: String): Unit ! W = Writer.tell(s)
  val step: Int ! W = for _ <- say("parse"); _ <- say("check") yield 42

  test("listen answers the value and what the scope told, and still tells it outward") {
    val p = for _ <- say("start"); heard <- Writer.listen[String, Int, Pure](step); _ <- say("end") yield heard
    assertEquals(!.run(Writer.run[String, (Int, Seq[String]), Pure](p)),
      (List("start", "parse", "check", "end"), (42, List("parse", "check"))))
  }

  test("listen tells in place: a tell before a raise reaches the outer Writer") {
    type R = Writer % String + Throws % String
    val failing: Int ! R = for _ <- say("a").at[R]; x <- raise[String, Int]("boom").at[R] yield x
    val p = Writer.run[String, Either[String, (Int, Seq[String])], Pure](runEither(Writer.listen[String, Int, Throws % String](failing)))
    assertEquals(!.run(p), (List("a"), Left("boom")))
  }

  test("censor rewrites the scope's whole output, and leaves the rest alone") {
    def summary(ws: Seq[String]): Seq[String] = if ws.size > 1 then Seq(s"${ws.size} steps") else ws
    val p = for _ <- say("start"); x <- Writer.censor[String, Int, Pure](step)(summary); _ <- say("end") yield x
    assertEquals(!.run(Writer.run[String, Int, Pure](p)), (List("start", "2 steps", "end"), 42))
  }

  test("censor holds the scope's tells back: a raise inside drops them") {
    type R = Writer % String + Throws % String
    val failing: Int ! R = for _ <- say("a").at[R]; x <- raise[String, Int]("boom").at[R] yield x
    val p = Writer.run[String, Either[String, Int], Pure](runEither(Writer.censor[String, Int, Throws % String](failing)(identity)))
    assertEquals(!.run(p), (List(), Left("boom")))
  }

  test("stack-safe over 100 000 tells") {
    val n = 100000
    val many: Unit ! W = (1 to n).foldLeft(pure[W, Unit](()))((acc, i) => acc.flatMap(_ => say(i.toString)))
    val (outer, (_, heard)) = !.run(Writer.run[String, (Unit, Seq[String]), Pure](Writer.listen[String, Unit, Pure](many)))
    assertEquals((outer.size, heard.size, heard.last), (n, n, n.toString))
    val (told, _) = !.run(Writer.run[String, Unit, Pure](Writer.censor[String, Unit, Pure](many)(ws => Seq(ws.size.toString))))
    assertEquals(told, List(n.toString))
  }
}
