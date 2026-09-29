package okay


/**
 * the pages under docs/effects/, VERBATIM (doc-snippet-debt): every example line of
 * the per-effect pages as the page prints it, answer comment included,
 * then asserted. A result the page shows is a `val` there too, so the
 * line compiles as it reads. The Async page is okay-platform's
 * TestDocExamplesAsync.
 */
class TestDocExamplesEffects extends munit.FunSuite:

  test("reader.md") {
    val greet: String ! Reader % String =
      Reader.ask[String].map(name => s"hello, $name")

    val hello = !.run(Reader.run("ada")(greet))   // "hello, ada"

    val shout = !.run(Reader.run("ada")(Reader.local[String, String, Pure](_.toUpperCase)(greet)))   // "hello, ADA"
    assertEquals(hello, "hello, ada")
    assertEquals(shout, "hello, ADA")
  }

  test("state.md") {
    val next: Int ! State % Int =
      for
        n <- State.get[Int]
        _ <- State.set(n + 1)
      yield n

    val twice = State.run(10)(next.flatMap(a => next.map(b => (a, b))))   // (12, (10, 11))

    val doubled = State.run(5)(State.modify[Int](_ * 2))   // (10, 10)
    assertEquals(twice, (12, (10, 11)))
    assertEquals(doubled, (10, 10))
  }

  test("writer.md") {
    val steps: Int ! Writer % String =
      for
        _ <- Writer.tell("parse")
        _ <- Writer.tell("check")
      yield 42

    val (log, answer) = !.run(Writer.collect(steps))   // (Vector(parse, check), 42)
    assertEquals((log, answer), (Vector("parse", "check"), 42))
  }

  test("throws.md") {
    def parse(s: String): Int ! Throws % String =
      s.toIntOption match
        case Some(n) => pure(n)
        case None    => raise(s"not a number: $s")

    val good = !.run(runEither(parse("42")))   // Right(42)
    val bad  = !.run(runEither(parse("x")))    // Left(not a number: x)

    val nothing = !.run(runOption(abort[Int]))   // None
    assertEquals(good, Right(42))
    assertEquals(bad, Left("not a number: x"))
    assertEquals(nothing, None)
  }

  test("maybe.md") {
    val ages = Map("ada" -> 36)
    def age(name: String): Int ! Maybe = ages.get(name).maybe

    val found   = !.run(Maybe.run(age("ada")))   // Some(36)
    val missing = !.run(Maybe.run(age("bob")))   // None
    assertEquals(found, Some(36))
    assertEquals(missing, None)
  }

  test("chronicle.md") {
    import Chronicle.Verdict.*
    def host(s: String): String ! Chronicle % String =
      if s.contains("_") then Chronicle.dictate(s"'$s' has an underscore").map(_ => s)
      else pure(s)

    val clean  = !.run(Chronicle.run(host("db")))      // Clean(db)
    val warned = !.run(Chronicle.run(host("my_db")))   // Warned(my_db, Vector('my_db' has an underscore))

    val failed = !.run(Chronicle.run(Chronicle.confess[String, String]("no host")))   // Failed(Vector(no host))
    assertEquals(clean, Clean("db"))
    assertEquals(warned, Warned("my_db", Vector("'my_db' has an underscore")))
    assertEquals(failed, Failed(Vector("no host")))
  }

  test("resource.md") {
    var log = Vector.empty[String]
    val sum: Int ! Resource =
      for
        a <- Resource.acquire({ log :+= "open a"; 1 })(_ => log :+= "close a")
        b <- Resource.acquire({ log :+= "open b"; 2 })(_ => log :+= "close b")
      yield a + b

    val three = Resource.scoped(sum)   // 3, and log is Vector(open a, open b, close b, close a)
    assertEquals(three, 3)
    assertEquals(log, Vector("open a", "open b", "close b", "close a"))
  }

  test("once.md") {
    var runs = 0
    val expensive: Int ! Once = Once.once[Int, Pure] { runs += 1; pure(21) }
    val both: Int ! Once = expensive.flatMap(a => expensive.map(b => a + b))

    val answer = !.run(Once.run(both))   // 42, and runs is 1
    assertEquals(answer, 42)
    assertEquals(runs, 1)
  }

  test("supply.md") {
    val three = for a <- Fresh.next; b <- Fresh.next; c <- Fresh.next yield List(a, b, c)

    val ids = !.run(Fresh.run(three))   // List(0, 1, 2)

    val (after, letters) = !.run(Supply.run('a')(c => (c + 1).toChar)(Supply.next[Char].flatMap(x => Supply.next[Char].map(y => s"$x$y"))))   // ('c', "ab")
    assertEquals(ids, List(0L, 1L, 2L))
    assertEquals((after, letters), ('c', "ab"))
  }

  test("choice.md") {
    val sums: Int ! Choose =
      for
        a <- choose(1, 2)
        b <- choose(10, 20)
      yield a + b

    val all = !.run(runChoice(sums))   // every branch: 11, 21, 12, 22

    val firstTwo = !.run(Logic.observe(2)(sums))   // the first two: 11, 21
    assertEquals(all.toList, List(11, 21, 12, 22))
    assertEquals(firstTwo.toList, List(11, 21))
  }

  test("gen.md") {
    val evens: Gen[Int] = Gen.from(1 to 10).filter(_ % 2 == 0)

    val firstThree = evens.take(3).toList   // List(2, 4, 6)

    val upToFour = Gen.from(1 to 10).flatMap(i => if i == 4 then Gen.stop else Gen.emit(i)).toList   // List(1, 2, 3)
    assertEquals(firstThree, List(2, 4, 6))
    assertEquals(upToFour, List(1, 2, 3))
  }

  test("prob.md") {
    import Prob.posterior
    val coins: Int ! Dist =
      for
        a <- Prob.uniform(0, 1)
        b <- Prob.uniform(0, 1)
      yield a + b

    val heads = !.run(Prob.runExact[Int, Pure](coins)).posterior   // Map(0 -> 0.25, 1 -> 0.5, 2 -> 0.25)

    val someHeads: Int ! Dist = coins.flatMap(n => Prob.observe(n > 0).map(_ => n))
    val conditioned = !.run(Prob.runExact[Int, Pure](someHeads)).posterior   // 1 -> 2/3, 2 -> 1/3
    assertEquals(heads, Map(0 -> 0.25, 1 -> 0.5, 2 -> 0.25))
    assertEqualsDouble(conditioned(1), 2.0 / 3, 1e-9, "P(1 | n > 0)")
    assertEqualsDouble(conditioned(2), 1.0 / 3, 1e-9, "P(2 | n > 0)")
  }
