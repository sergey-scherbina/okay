package okay.cont

import Machine.value

/** specs/freer-min.md, stage 35: the machine's effect library — handlers as delimiters, doors as fragments */
class TestEffects extends okay.testkit.Munit.Diagnosed:

  test("state: get, put, modify; the last state and the value"):
    val p = state[Int, Int](0):
      for
        a <- get[Int]
        _ <- put(a + 41)
        b <- modify[Int](_ + 1)
      yield b
    assertEquals(value(p), (42, 42))

  test("reader: ask and asks, answered in place"):
    assertEquals(value(reader[Int, Int](10)(ask[Int].flatMap(r => asks[Int, Int](_ * 2).map(_ + r)))), 30)

  test("writer: the log in order, and the value"):
    assertEquals(value(writer[String, Int](tell("a").flatMap(_ => tell("b")).map(_ => 1))), (List("a", "b"), 1))

  test("throws: Right on the way through, Left at the first raise, the rest never run"):
    var after = 0
    assertEquals(value(throws[String, Int](pure(1))), Right(1))
    assertEquals(value(throws[String, Int](raise[String, Int]("boom").map(x => { after += 1; x }))), Left("boom"))
    assertEquals(after, 0)

  test("an abort through a delimiter between: state keeps what was put before the raise"):
    val p = state[Int, Either[String, Int]](0):
      throws[String, Int]:
        put(1).flatMap(_ => raise[String, Int]("x")).map(x => x + 1)
    assertEquals(value(p), (1, Left("x")))

  test("choose: every path, in order; a cell shared by the paths sees one state"):
    assertEquals(value(choose[Int](among(Seq(1, 2)).flatMap(a => among(Seq(10, 20)).map(a + _)))), Seq(11, 21, 12, 22))
    val shared = state[Int, Seq[Int]](0):
      choose[Int](among(Seq(1, 2, 3)).flatMap(x => modify[Int](_ + x)))
    assertEquals(value(shared), (6, Seq(1, 3, 6)))

  test("an operation inside two delimiters reaches its handler outside: reader under state"):
    val p = state[Int, Int](0):
      reader[Int, Int](5):
        ask[Int].flatMap(r => put(r)).flatMap(_ => get[Int])
    assertEquals(value(p), (5, 5))

  test("collect: every element yielded, in place"):
    assertEquals(value(collect[Int, Unit](yield_(1).flatMap(_ => yield_(2)))), (List(1, 2), ()))

  test("generate: lazy — nothing past a yield runs until the consumer pulls"):
    var ran = 0
    val g: Gen[Int, EmptyTuple] = value(generate[Int](yield_(1).flatMap(_ => { ran += 1; yield_(2) }).map(_ => { ran += 1; () })))
    g match
      case Gen.Next(1, rest) =>
        assertEquals(ran, 0)
        value(rest) match
          case Gen.Next(2, rest2) =>
            assertEquals(ran, 1)
            assertEquals(value(rest2), Gen.Done[Int, EmptyTuple]())
            assertEquals(ran, 2)
          case other => fail(s"not the second: $other")
      case other => fail(s"not the first: $other")

  test("dialogue: a question captures the rest; driven, and resumed twice from one pause"):
    val d = dialogue[String, Int, Int](question[String, Int]("a").flatMap(x => question[String, Int]("bb").map(_ + x)))
    assertEquals(Paused.drive(value(d))(q => q.length * 10), 30)
    value(d) match
      case Paused.Ask("a", resume) =>
        assertEquals(Paused.drive(value(resume(1)))(_ => 100), 101)
        assertEquals(Paused.drive(value(resume(2)))(_ => 100), 102)
      case other => fail(s"not paused at a: $other")

  test("100 000 operations of each kind in constant stack"):
    def loop(n: Int, acc: Int)(using c: In[(Int, Int), Root.type], p: Perform[[X] =>> State[Int, X], c.type]): c.Body[Int] =
      if n == 0 then pure(acc) else modify[Int](_ + 1).flatMap(x => loop(n - 1, acc + x))
    assertEquals(value(state[Int, Int](0)(loop(100000, 0)))._1, 100000)
    def tells(n: Int)(using c: In[(List[Int], Int), Root.type], p: Perform[[X] =>> Writer[Int, X], c.type]): c.Body[Int] =
      if n == 0 then pure(0) else tell(n).flatMap(_ => tells(n - 1).map(_ + 1))
    assertEquals(value(writer[Int, Int](tells(100000)))._2, 100000)
