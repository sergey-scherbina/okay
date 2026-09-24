package okay

import Bisim.{Answers, Verdict}
import okay.Row.at

/**
 * specs/handler-equivalence-oracle.md — the oracle's own controls
 * first (it must be able to say Differ, and a Same must count what it
 * saw), then what it is for: the stage laws a `Gen` chain rewrite
 * relies on, checked on the materialised programs.
 */
class TestBisim extends munit.FunSuite:

  type Mixed = State % Int + Writer % String

  given Answers[State % Int] = Answers.state(0, 1, 2)

  def same(v: Verdict, clue: String = ""): Unit =
    assert(v.same, s"$clue $v")

  // ---------------------------------------------------------------- controls

  test("CONTROL: a program is the same as itself, and the count says how many paths were walked") {
    val p = State.get[Int].flatMap(s => State.set(s + 1))
    assertEquals(Bisim.check(p, p), Verdict.Same(3, 0))
  }

  test("CONTROL: a difference on ONE answer is found, and the path to it is printed") {
    val p = State.get[Int].flatMap(s => State.set(s))
    val q = State.get[Int].flatMap(s => State.set(if s == 2 then 0 else s))
    Bisim.check(p, q) match
      case Verdict.Differ(path, l, r) =>
        assertEquals(path, List("Get() -> 2"))
        assertEquals((l, r), ("performed Set(2)", "performed Set(0)"))
      case v => fail(s"expected Differ, got $v")
  }

  test("CONTROL: the answers are a SAMPLE — the same pair passes when 2 is not in it") {
    val p = State.get[Int].flatMap(s => State.set(s))
    val q = State.get[Int].flatMap(s => State.set(if s == 2 then 0 else s))
    assertEquals(Bisim.check(p, q)(using Answers.state(0, 1)), Verdict.Same(2, 0))
  }

  test("CONTROL: returning and performing are told apart, and so are two values") {
    val stop: Unit ! Gen.Row[Int] = Gen.stop[Int].program
    val done: Unit ! Gen.Row[Int] = Gen.empty[Int].program
    assert(!Bisim.check(stop, done).same)
    assert(!Bisim.check(State.get[Int].map(_ + 1), State.get[Int].map(_ + 2)).same)
  }

  test("CONTROL: a program that never ends is CUT at the depth bound, not called equal by default") {
    def spin: Unit ! State % Int = State.get[Int].flatMap(_ => spin)
    // three answers per get, five gets deep: 3^5 paths, every one cut
    assertEquals(Bisim.check(spin, spin, depth = 5), Verdict.Same(0, 243))
    // and the budget bounds a tree the depth alone would not
    Bisim.check(spin, spin, depth = 1_000, budget = 50) match
      case Verdict.Same(paths, cut) => assert(paths == 0 && cut >= 50, s"$paths $cut")
      case v => fail(v.toString)
  }

  test("SCOPE: `get; get` and `get` differ FREELY, and agree after State's own handler") {
    given Answers[Mixed] = Answers.state(0, 1, 2) + Answers.writer[String]
    val twice: Unit ! Mixed = for
      a <- State.get[Int].at[Mixed]
      b <- State.get[Int].at[Mixed]
      _ <- Writer.tell(s"$a$b").at[Mixed]
    yield ()
    val once: Unit ! Mixed = for
      a <- State.get[Int].at[Mixed]
      _ <- Writer.tell(s"$a$a").at[Mixed]
    yield ()
    assert(!Bisim.check(twice, once).same, "a handler may answer the two gets differently")
    given Answers[Writer % String] = Answers.writer[String]
    same(Bisim.check(State.handle[Int](7)(twice), State.handle[Int](7)(once)), "under State.handle")
  }

  // ------------------------------------------------ what it is for: Gen stage laws

  given Answers[Gen.Row[Int]] = Answers.writer[Int] + Answers.stop

  val src: Gen[Int] = Gen.from(0 until 20)
  val f: Int => Int = _ * 3
  val g: Int => Int = _ + 1
  val even: Int => Boolean = _ % 2 == 0
  val small: Int => Boolean = _ < 30

  def law(name: String)(l: Gen[Int], r: Gen[Int]): Unit =
    test(s"GEN LAW: $name") {
      val v = Bisim.check(l.program, r.program, depth = 64)
      assert(v.same, v.toString)
      v match
        case Verdict.Same(paths, cut) => assertEquals((paths, cut), (1, 0), "one path, walked to its end")
        case _ => ()
    }

  law("map . map = map (f andThen g)")(src.map(f).map(g), src.map(f andThen g))
  law("filter . filter = filter (p && q)")(src.filter(even).filter(small), src.filter(x => even(x) && small(x)))
  law("drop n . drop m = drop (n + m)")(src.drop(3).drop(4), src.drop(7))
  law("map then filter = filter (p . f) then map")(src.map(f).filter(even), src.filter(x => even(f(x))).map(f))
  law("flatMap emit = identity")(src.flatMap(Gen.emit), src)

  test("GEN: take n . take m and take (min n m) — the oracle's reading, pinned") {
    val v = Bisim.check(src.take(5).take(3).program, src.take(3).program, depth = 64)
    assert(v.same, v.toString)
  }

  test("GEN MUTANT: a misstated law — filter and map swapped with p read BEFORE f — is refused with its path") {
    val v = Bisim.check(src.map(g).filter(even).program, src.filter(even).map(g).program, depth = 64)
    v match
      case Verdict.Differ(path, l, r) =>
        assertEquals((l, r), ("performed Say(2)", "performed Say(1)"))
        assertEquals(path, Nil, "the first element already differs")
      case _ => fail(s"expected Differ, got $v")
  }

  // A LAW relates a stage to ITSELF, so a stage that is wrong the same way
  // on both sides can pass it: `taking` mutated to count down by two kept
  // take.take green and failed "take" below (measured, specs Results). A
  // MODEL relates a stage to what it should mean — the same operation on
  // the List the source holds.
  val xs: List[Int] = (0 until 20).toList

  def model(name: String)(stage: Gen[Int], expected: List[Int]): Unit =
    test(s"GEN MODEL: $name") {
      val v = Bisim.check(stage.program, Gen.from(expected).program, depth = 64)
      assertEquals(v, Verdict.Same(1, 0))
    }

  model("take")(src.take(3), xs.take(3))
  model("take 0")(src.take(0), Nil)
  model("drop")(src.drop(3), xs.drop(3))
  model("takeWhile")(src.takeWhile(small compose f), xs.takeWhile(small compose f))
  model("filter")(src.filter(even), xs.filter(even))
  model("map")(src.map(f), xs.map(f))
  model("zipWithIndex, second component")(src.map(g).zipWithIndex.map(_._2), xs.indices.toList)
