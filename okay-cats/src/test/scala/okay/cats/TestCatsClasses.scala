package okay.cats

import _root_.cats.Eq
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import _root_.cats.laws.discipline.ApplicativeTests
import _root_.cats.syntax.all.*
import okay.{!, Async, Choose, Static, Validated, async, choose, runChoice}
import okay.given
import okay.==>
import org.scalacheck.{Arbitrary, Gen}
import java.util.concurrent.{CountDownLatch, TimeUnit}

/**
 * The default instances of CatsClasses.scala (specs/interop-classes.md):
 * cats' classes over okay's non-monadic carriers, okay's over cats'
 * types. Each test checks the carrier KEPT its point under the other
 * library's combinators — accumulation, staticness, parallelism, a
 * skipped handler — not merely that an instance resolved.
 */
class TestCatsClasses extends munit.ScalaCheckSuite with okay.testkit.Munit.Diagnosed {

  private def checkAll(name: String, rules: org.typelevel.discipline.Laws#RuleSet): Unit =
    for (id, prop) <- rules.all.properties do property(s"$name: $id")(prop)

  // ---- okay.Validated under cats

  type V[A] = Validated[String, A]

  given [A]: Eq[V[A]] = Eq.fromUniversalEquals
  given [A](using a: Arbitrary[A]): Arbitrary[V[A]] = Arbitrary(Gen.oneOf(
    a.arbitrary.map(x => Validated.valid[String, A](x)),
    Gen.alphaStr.map(e => Validated.invalid[String, A](e)),
  ))

  checkAll("cats.Applicative[okay.Validated]", ApplicativeTests[V].applicative[Int, Int, String])

  test("cats' traverse over okay.Validated reports EVERY error") {
    val check = (i: Int) =>
      if i % 2 == 1 then Validated.invalid[Vector[String], Int](Vector(s"odd $i"))
      else Validated.valid[Vector[String], Int](i)
    val out = List(1, 2, 3, 4, 5).traverse(check)
    note(s"traverse answered $out")
    assertEquals(out, Validated.invalid(Vector("odd 1", "odd 3", "odd 5")))
    assertEquals(List(2, 4).traverse(check), Validated.valid(List(2, 4)))
  }

  test("cats' mapN over okay.Validated combines both failures") {
    val a = Validated.invalid[String, Int]("a")
    val b = Validated.invalid[String, Int]("b")
    assertEquals((a, b).mapN(_ + _), Validated.invalid("ab"))
  }

  // ---- okay.Static under cats

  enum Op[+A]:
    case Get(key: String) extends Op[Int]

  test("cats' traverse over Static stays static: operations listed before running") {
    val s = List("a", "b", "c").traverse(k => Static.op(Op.Get(k)))
    assertEquals(s.leaves.toList, List(Op.Get("a"), Op.Get("b"), Op.Get("c")))
    val nt: Op ==> Option = [X] => (o: Op[X]) => o match
      case Op.Get(k) => Some(k.head.toInt)
    assertEquals(s.foldMap[Option](nt), Some(List(97, 98, 99)))
  }

  // ---- Par as cats' Parallel

  private def leaf(latch: CountDownLatch, millis: Long): Boolean ! Async =
    async { latch.countDown(); latch.await(millis, TimeUnit.MILLISECONDS) }

  test("cats' parTraverse over A ! Async forks: four leaves that must meet, meet") {
    val latch = CountDownLatch(4)
    val p: List[Boolean] ! Async = List(1, 2, 3, 4).parTraverse(_ => leaf(latch, 10000))
    assertEquals(p.runWith, List.fill(4)(true))
  }

  test("cats' traverse (the monad) over the same leaves does not fork — the control") {
    val latch = CountDownLatch(2)
    val p: List[Boolean] ! Async = List(1, 2).traverse(_ => leaf(latch, 200))
    assertEquals(p.runWith.head, false)
  }

  test("parMapN answers in the arguments' order") {
    val p: (Int, String) ! Async = (async(1), async("x")).parMapN((a, b) => (a, b))
    assertEquals(p.runWith, (1, "x"))
  }

  // ---- A ! Choose as cats' Alternative

  test("cats' MonoidK over A ! Choose: <+> is choice, empty prunes") {
    val both = choose(1, 2) <+> choose(3)
    assertEquals(!.run(runChoice(both)), Seq(1, 2, 3))
    val A = CatsClasses.chooseAlternative
    val pruned = choose(1, 2, 3, 4).flatMap(i => if i % 2 == 0 then A.pure(i) else A.empty[Int])
    assertEquals(!.run(runChoice(pruned)), Seq(2, 4))
    assertEquals(!.run(runChoice(A.guard(false).map(_ => 1))), Seq.empty)
  }

  test("cats' traverse over A ! Choose still resolves — the MonoidK did not tie with the monad") {
    val p = List(1, 2).traverse(i => choose(i, -i))
    assertEquals(!.run(runChoice(p)), Seq(List(1, 2), List(1, -2), List(-1, 2), List(-1, -2)))
  }

  // ---- okay's classes over cats' types

  type CV[A] = _root_.cats.data.Validated[String, A]

  test("okay.Selective over cats' Validated: select SKIPS the handler on Right") {
    val S = summon[okay.Selective[CV]]
    var ran = 0
    def handler: CV[Int => Int] = { ran += 1; _root_.cats.data.Validated.Valid(_ + 1) }
    val right: CV[Either[Int, Int]] = _root_.cats.data.Validated.Valid(Right(7))
    assertEquals(S.select(right)(handler), _root_.cats.data.Validated.Valid(7))
    assertEquals(ran, 0)
    val left: CV[Either[Int, Int]] = _root_.cats.data.Validated.Valid(Left(7))
    assertEquals(S.select(left)(handler), _root_.cats.data.Validated.Valid(8))
    assertEquals(ran, 1)
  }

  test("okay.traverse over cats' Validated accumulates") {
    val out = okay.traverse(Seq(1, 2, 3))(i =>
      if i == 2 then _root_.cats.data.Validated.Valid(i): CV[Int]
      else _root_.cats.data.Validated.Invalid(s"<$i>"): CV[Int])
    assertEquals(out, _root_.cats.data.Validated.Invalid("<1><3>"))
  }

  test("okay.traverse and whenS over cats' IO") {
    assertEquals(okay.traverse(Seq(1, 2, 3))(i => IO(i * 2)).unsafeRunSync(), Seq(2, 4, 6))
    var fired = 0
    val io = okay.whenS(IO(true))(IO { fired += 1 }).flatMap(_ => okay.whenS(IO(false))(IO { fired += 10 }))
    io.unsafeRunSync()
    assertEquals(fired, 1)
  }

  test("okay.traverse over cats' Eval is stack-safe at depth") {
    val n = 100000
    val out = okay.traverse(1 to n)(i => _root_.cats.Eval.later(i)).value
    assertEquals(out.length, n)
  }

  test("foldMap: a program's operations interpreted straight into IO") {
    enum Ask[+A]:
      case Num(k: String) extends Ask[Int]
    val p: Int ! Ask = for
      a <- okay.effect(Ask.Num("a"))
      b <- okay.effect(Ask.Num("bb"))
    yield a * 10 + b
    val toIO: Ask ==> IO = [X] => (e: Ask[X]) => e match
      case Ask.Num(k) => IO(k.length)
    assertEquals(p.foldMap(toIO).unsafeRunSync(), 12)
  }

  test("conversions: okay.Validated <-> cats' Validated, both roads") {
    val ok = Validated.valid[String, Int](1)
    val bad = Validated.invalid[String, Int]("e")
    assertEquals(CatsInterop.fromCatsValidated(CatsInterop.toCatsValidated(ok)), ok)
    assertEquals(CatsInterop.toCatsValidated(bad), _root_.cats.data.Validated.Invalid("e"))
  }
}
