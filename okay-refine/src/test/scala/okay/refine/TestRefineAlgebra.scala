package okay.refine

import okay.{Optic, through}

import okay.freer.{%}
import okay.freer.{!, Writer, pure, runEither, runChoice}
import okay.freer.given
import okay.codec.Json
import okay.testkit.Munit.Diagnosed

/** specs/refine.md, refine-algebra: the glyphs, the classes a pattern IS
 * an instance of (with their laws), and the roads into effects and streams */
class TestRefineAlgebra extends Diagnosed:

  val int: Refine[String, Int] =
    Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)
  val even: Refine[Int, Int] =
    Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left(s"$n is odd"))(identity)
  val small: Refine[Int, Int] =
    Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left(s"$n is not small"))(identity)
  val big: Refine[Int, Int] =
    Refine.step[Int, Int]("big")(n => if n >= 100 then Right(n) else Left(s"$n is not big"))(identity)
  val half: Refine[Int, Int] =
    Refine.step[Int, Int]("half")(n => if n % 2 == 0 then Right(n / 2) else Left(s"$n has no half"))(_ * 2)

  /** what "the same pattern" means: the same verdict on every input, the same write on every value */
  def same[A, B](x: Refine[A, B], y: Refine[A, B], inputs: Seq[A], values: Seq[B]): Unit =
    for a <- inputs do assertEquals(x.run(a), y.run(a), s"run($a)")
    for b <- values do assertEquals(x.write(b), y.write(b), s"write($b)")

  val strings = Seq("4", "7", "11", "200", "x", "")
  val ints = Seq(-3, 0, 4, 7, 11, 200)

  test(">>> is andThen, or is <|>: the same pattern under another name") {
    same(int >>> even, int andThen even, strings, ints)
    same(even or small or big, even <|> small <|> big, ints, ints)
  }

  test("Category: id adds no name, is a unit on both sides, and >>> is associative (Took, Unclear and Declined)") {
    val choice = even <|> small <|> big
    same(Refine.id[String] >>> int, int, strings, ints)
    same(int >>> Refine.id[Int], int, strings, ints)
    same((int >>> choice) >>> half, int >>> (choice >>> half), strings, ints)
    same((int >>> even) >>> half, int >>> (even >>> half), strings, ints)
    assertEquals((Refine.id[String] >>> int).name, "int")
    // and through the class, as generic code sees it
    val C = summon[Optic.Category[Refine]]
    same(C.compose(half, C.compose(even, int)), int >>> even >>> half, strings, ints)
    same(C.compose(C.id[Int], even), even, ints, ints)
  }

  test("Monoid under or: empty is a unit on both sides, and or is associative") {
    val e = Refine.empty[Int, Int]
    same(e or even, even, ints, ints)
    same(even or e, even, ints, ints)
    same((even or small) or big, even or (small or big), ints, ints)
    assertEquals(e.run(4), Verdict.Declined(Vector.empty))
    assertEquals(e.write(4), Left(": no alternative writes this value"))
  }

  test("orElse: the fallback runs only when the first DECLINES, and cannot make the answer Unclear") {
    var fallbackRuns = 0
    val counting: Refine[Int, Int] = Refine.step[Int, Int]("small")(n => { fallbackRuns += 1; small.run(n).toOption.toRight("not small") })(identity)
    // 4 is even AND small: `or` says Unclear, `orElse` answers with even and never asks small
    assert((even or small).run(4).isInstanceOf[Verdict.Unclear[?]])
    assertEquals((even orElse counting).run(4), Verdict.Took(4, Path("even"), Vector.empty))
    assertEquals(fallbackRuns, 0)
    // 7: even declines, the fallback takes, and the refusal that sent us there is kept
    assertEquals((even orElse counting).run(7), Verdict.Took(7, Path("small"), Vector(Refusal(Path("even"), "7 is odd"))))
    assertEquals(fallbackRuns, 1)
    // both decline: every reason, in order
    assertEquals((even orElse small).run(11).reasons.map(_.reason), Vector("11 is odd", "11 is not small"))
    // write: the first's, else the fallback's
    val num: Refine[String, AnyVal] = int.widen[AnyVal] orElse Refine.step[String, Double]("double")(s => s.toDoubleOption.toRight("no"))(_.toString).widen[AnyVal]
    assertEquals(num.write(42), Right("42"))
    assertEquals(num.write(4.5), Right("4.5"))
  }

  test("orElse is a monoid too: empty is its unit, and it is associative") {
    val e = Refine.empty[Int, Int]
    same(e orElse even, even, ints, ints)
    same(even orElse e, even, ints, ints)
    same((even orElse small) orElse big, even orElse (small orElse big), ints, ints)
  }

  test("***: each half through its own pattern, the path left then right, readings multiplied") {
    val pair = int *** int
    assertEquals(pair.run(("4", "7")), Verdict.Took((4, 7), Path("int", "int"), Vector.empty))
    assertEquals(pair.run(("4", "x")), Verdict.Declined(Vector(Refusal(Path("int"), "'x' is not an integer"))))
    assertEquals(pair.write((4, 7)), Right(("4", "7")))
    // 4 is even and small on the left: two readings times one on the right
    (int >>> (even or small)) *** int match
      case p => p.run(("4", "1")) match
        case Verdict.Unclear(cs, _) => assertEquals(cs.map(_._1), Vector(Path("int", "even", "int"), Path("int", "small", "int")))
        case other => fail(s"expected Unclear, got $other")
  }

  test("+++: the Left through one pattern, the Right through the other, each written back through its own side") {
    val s = int +++ even
    assertEquals(s.run(Left("42")), Verdict.Took(Left(42), Path("int"), Vector.empty))
    assertEquals(s.run(Right(7)), Verdict.Declined(Vector(Refusal(Path("even"), "7 is odd"))))
    assertEquals(s.write(Right(8)), Right(Right(8)))
    assertEquals(s.write(Left(3)), Right(Left("3")))
  }

  test("and: a record read field by field over ONE input, written back by merging the skeletons") {
    import Refine.json.*
    val money = (field("amount") >>> num) and (field("currency") >>> str)
    val j = Json.parse("""{"amount": 5, "currency": "EUR"}""")
    assertEquals(money.run(j), Verdict.Took((5.0, "EUR"), Path("amount", "number", "currency", "string"), Vector.empty))
    assertEquals(money.write((5.0, "EUR")).map(Json.print), Right("""{"amount":5,"currency":"EUR"}"""))
    // the prism law, through the product
    assertEquals(money.write((5.0, "EUR")).map(money.run).flatMap(_.toOption.toRight("?")), Right((5.0, "EUR")))
    // a missing half declines in its own words
    assertEquals(money.run(Json.parse("""{"amount": 5}""")).reasons.map(_.reason), Vector("no field `currency`"))
    // two halves writing the same field differently refuse to merge
    val clash = (field("a") >>> num) and (field("a") >>> num)
    assertEquals(clash.write((1.0, 2.0)), Left("both halves write field `a`, differently"))
  }

  test("orRaise: the read as an effect — the value, or the whole verdict raised and handed back by runEither") {
    assertEquals(!.run(runEither[Int, okay.freer.Pure, Verdict[Int]](int.orRaise("42"))), Right(42))
    !.run(runEither[Int, okay.freer.Pure, Verdict[Int]](int.orRaise("x"))) match
      case Left(Verdict.Declined(rs)) => assertEquals(rs.map(_.reason), Vector("'x' is not an integer"))
      case other => fail(s"expected the verdict raised, got $other")
    // inside a program: two reads, the first failure stops it
    val both = for a <- int.orRaise("4"); b <- int.orRaise("five") yield a + b
    assert(!.run(runEither[Int, okay.freer.Pure, Verdict[Int]](both)).isLeft)
  }

  test("search: the pattern as a Choose program — Took one answer, Unclear a choice point, Declined none") {
    val choice = int >>> (even or small or big)
    def readings(s: String): Seq[Int] = !.run(runChoice[Int, okay.freer.Pure](choice.search(s)))
    assertEquals(readings("7"), Seq(7))
    assertEquals(readings("4"), Seq(4, 4))
    assertEquals(readings("11"), Seq())
  }

  test("streams: verdicts is one verdict per input; taken emits the values and ANSWERS what it did not take") {
    val choice = int >>> (even or small)
    val inputs: Unit ! Writer % String =
      Seq("4", "7", "11", "x", "200").foldLeft(pure[Writer % String, Unit](()))((m, s) => m.flatMap(_ => Writer.tell(s)))
    val (vs, _) = !.run(Writer.run(through(inputs)(choice.verdicts)))
    assertEquals(vs.map {
      case Verdict.Took(b, _, _) => s"took $b"
      case Verdict.Unclear(cs, _) => s"unclear ${cs.length}"
      case Verdict.Declined(_) => "declined"
    }, Seq("unclear 2", "took 7", "declined", "declined", "took 200"))
    val (values, missed) = !.run(Writer.run(through(inputs)(choice.taken)))
    assertEquals(values, Seq(7, 200))
    assertEquals(missed, Refine.Missed(declined = 2, unclear = 1))
  }

  test("path: the category's fold, named — path() is id, path(a, b, c) is a >>> b >>> c, no name added") {
    val double: Refine[Int, Int] = Refine.step[Int, Int]("double")(n => Right(n * 2))(_ / 2)
    same(Refine.path[Int](), Refine.id[Int], ints, ints)
    same(Refine.path(even, double, half), even >>> double >>> half, ints, ints)
    assertEquals(Refine.path(even, double).run(4), Verdict.Took(8, Path("even", "double"), Vector.empty))
    assertEquals(Refine.path(even, double).write(8), Right(4))
  }

  test("json.at: a descent through fields, each field a step of the verdict's path, written back as a skeleton") {
    import Refine.json.*
    val deep = Json.parse("""{"dataDocument": {"trade": {"swap": {"id": "s1"}}}}""")
    assertEquals(at("dataDocument", "trade", "swap").run(deep).toOption, Some(Json.parse("""{"id": "s1"}""")))
    assertEquals(at("dataDocument", "trade", "swap").run(deep) match { case Verdict.Took(_, by, _) => by; case _ => Path.empty },
      Path("dataDocument", "trade", "swap"))
    assertEquals((at("dataDocument", "trade", "fx") >>> str).run(deep).reasons.map(_.reason), Vector("no field `fx`"))
    assertEquals(at("a", "b").write(Json.JStr("x")).map(Json.print), Right("""{"a":{"b":"x"}}"""))
    assertEquals(at().run(deep).toOption, Some(deep))
  }

