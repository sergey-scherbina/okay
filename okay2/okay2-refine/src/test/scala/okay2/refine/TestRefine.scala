package okay2.refine

/** okay's specs/refine.md, stage 1, on the Scala 2 core: a step, a path,
 * a choice, the way back, and the prism a step is */
class TestRefine extends munit.FunSuite {

  val int: Refine.Step[String, Int] =
    Refine.Step[String, Int]("int", s => s.toIntOption.toRight(s"'$s' is not an integer"), _.toString)
  val even: Refine[Int, Int] =
    Refine.step[Int, Int]("even")(n => if (n % 2 == 0) Right(n) else Left(s"$n is odd"))(identity)
  val small: Refine[Int, Int] =
    Refine.step[Int, Int]("small")(n => if (n < 10) Right(n) else Left(s"$n is not small"))(identity)
  val big: Refine[Int, Int] =
    Refine.step[Int, Int]("big")(n => if (n >= 100) Right(n) else Left(s"$n is not big"))(identity)

  test("a step that reads takes, under its name, with nothing declined") {
    assertEquals(int.run("42"), Verdict.Took(42, Path("int"), Vector.empty))
  }

  test("a step that refuses declines with its reason under its name") {
    assertEquals(int.run("x"), Verdict.Declined(Vector(Refusal(Path("int"), "'x' is not an integer"))))
  }

  test("andThen: the path is both names; a refusal in the second is under the composed path") {
    assertEquals((int andThen even).run("42"), Verdict.Took(42, Path("int", "even"), Vector.empty))
    assertEquals((int andThen even).run("7"), Verdict.Declined(Vector(Refusal(Path("int", "even"), "7 is odd"))))
  }

  test("<|>: one taker is Took with the others declined; two is Unclear; none is Declined with every reason") {
    val choice = even <|> small <|> big
    assertEquals(choice.name, "even|small|big")
    assertEquals((small <|> big).run(4), Verdict.Took(4, Path("small"), Vector(Refusal(Path("big"), "4 is not big"))))
    choice.run(4) match {
      case Verdict.Unclear(cs, d) =>
        assertEquals(cs, Vector((Path("even"), 4), (Path("small"), 4)))
        assertEquals(d, Vector(Refusal(Path("big"), "4 is not big")))
      case other => fail(s"expected Unclear (4 is even and small), got $other")
    }
    choice.run(200) match {
      case Verdict.Unclear(cs, _) => assertEquals(cs.map(_._1), Vector(Path("even"), Path("big")))
      case other => fail(s"expected Unclear (200 is even and big), got $other")
    }
    assertEquals(choice.run(11), Verdict.Declined(Vector(
      Refusal(Path("even"), "11 is odd"), Refusal(Path("small"), "11 is not small"), Refusal(Path("big"), "11 is not big"))))
  }

  test("an Unclear before a path: every candidate goes on through the second pattern") {
    val evenOrSmall = int andThen (even <|> small)
    assertEquals(evenOrSmall.run("7"), Verdict.Took(7, Path("int", "small"), Vector(Refusal(Path("int", "even"), "7 is odd"))))
    val thenBig = (int andThen (even <|> small)) andThen big
    assertEquals(thenBig.run("4"), Verdict.Declined(Vector(
      Refusal(Path("int", "even", "big"), "4 is not big"), Refusal(Path("int", "small", "big"), "4 is not big"))))
    assertEquals(thenBig.run("200").toOption, Some(200))
  }

  test("write follows the path back; a lossless step round-trips") {
    assertEquals((int andThen even).write(42), Right("42"))
    assertEquals(int.write(int.run("42").toOption.get), Right("42"))
  }

  test("widen into a sum: read is the case, write takes that case only, Or.write asks in order") {
    val s: Refine[String, Int] = int
    val d: Refine[String, Double] =
      Refine.step[String, Double]("double")(x => x.toDoubleOption.filter(_ => x.contains('.')).toRight(s"'$x' is not a decimal"))(_.toString)
    val num: Refine[String, AnyVal] = s.widen[AnyVal] <|> d.widen[AnyVal]
    assertEquals(num.run("42").toOption, Some(42: AnyVal))
    assertEquals(num.run("4.5").toOption, Some(4.5: AnyVal))
    assertEquals(num.write(42), Right("42"))
    assertEquals(num.write(4.5), Right("4.5"))
    assertEquals(num.write(true), Left("int|double: no alternative writes this value"))
  }

  test("a step's prism obeys the prism laws: review then preview is identity; preview then review is identity where it previews") {
    import okay2.optics.Optic._
    val p = int.prism
    assertEquals(p.preview("42"), Some(42))
    assertEquals(p.preview("x"), None)
    assertEquals(p.set(7)("42"), "7")
    assertEquals(p.set(7)("x"), "x")
    assertEquals(p.preview(p.set(7)("42")), Some(7))
  }

  test("map: an iso on what is learnt, and the path still writes") {
    val neg = int.map("neg")((n: Int) => -n, (n: Int) => -n)
    assertEquals(neg.run("5"), Verdict.Took(-5, Path("int"), Vector.empty))
    assertEquals(neg.write(-5), Right("5"))
  }

  test("search: a taker is the answer, Unclear a choice point, Declined an empty one") {
    import okay2._
    val choice = int andThen (even <|> small <|> big)
    def readings(s: String): Seq[Int] = !.run(runChoice[Int, Pure](choice.search(s)))
    assertEquals(readings("7"), Seq(7))
    assertEquals(readings("4"), Seq(4, 4))
    assertEquals(readings("11"), Seq())
  }
}
