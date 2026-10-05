package okay2

import Condition._
import Condition.Decision._
import TestConditionTypedFixture.{Damaged, HowMany => HowManyCase}

/** the condition battery (okay2-condition-repair, the Scala 3 core's TestCondition): resume at the point, unwind to
 * the frame, the menu, escalation, forwarding — and the repair story: one program, three outcomes, chosen at run */
class TestCondition extends munit.FunSuite {

  type C = Condition

  test("the lexical invoke unwinds to ITS frame; no handle, no invoke") {
    val prog: String ! C =
      frame[String, Int, Pure]("skip") { restart =>
        pure[C, Int](1).flatMap(_ => restart.invoke[String](42)).map(_ => "never reached")
      }(v => s"skipped with $v")
    assertEquals(!.run(Condition.run[String, Pure]((_, _) => throw new AssertionError("the policy must not be consulted"))(prog)),
      "skipped with 42")
    val nested: String ! C =
      frame[String, String, Pure]("outer") { _ =>
        frame[String, Int, Pure]("inner") { inner => inner.invoke[String](7) }(v => s"inner=$v").map(x => x + ", outer went on")
      }(v => s"outer=$v")
    assertEquals(!.run(Condition.run[String, Pure]((_, _) => Fail)(nested)), "inner=7, outer went on")
    val crossing: String ! C =
      frame[String, String, Pure]("outer") { outer =>
        frame[String, Int, Pure]("inner") { _ => outer.invoke[String]("all the way out") }(v => s"inner=$v")
          .map(x => x + ", never appended")
      }(v => s"outer=$v")
    assertEquals(!.run(Condition.run[String, Pure]((_, _) => Fail)(crossing)), "outer=all the way out")
    assert(compileErrors("implicitly[okay2.Condition.Restart[Int]]").nonEmpty, "a Restart outside every frame must not summon")
  }

  test("a handle targets ITS frame by identity — two frames of one name cannot alias") {
    val aliased: String ! C =
      frame[String, Int, Pure]("retry") { outer =>
        frame[String, String, Pure]("retry") { _ => outer.invoke[String](3) }(s => s"inner got ${s.length}")
          .map(x => x + ", never appended")
      }(n => s"outer got $n")
    assertEquals(!.run(Condition.run[String, Pure]((_, _) => Fail)(aliased)), "outer got 3")
    val byName: String ! C =
      frame[String, Int, Pure]("retry") { _ =>
        frame[String, String, Pure]("retry") { _ => signal[String]("which?") }(s => s"inner got $s")
      }(n => s"outer got $n")
    assertEquals(!.run(Condition.run[String, Pure]((_, _) => Invoke("retry", "x"))(byName)), "inner got x")
  }

  test("the typed pair: raiseC answers its instance's type; a bad Resume is the policy's bug, named") {
    implicit val answers: Answers[HowManyCase, Int] = Answers.of[HowManyCase, Int]
    val prog: Int ! C = raiseC[HowManyCase, Int](HowManyCase("retries")).map(_ * 2)
    assertEquals(!.run(Condition.run[Int, Pure]((_, _) => Resume(21))(prog)), 42)
    val bad = intercept[BadResume](!.run(Condition.run[Int, Pure]((_, _) => Resume("twenty-one"))(prog)))
    assert(bad.getMessage.contains("HowMany"), bad.getMessage)
    assert(bad.getMessage.contains("not a"), bad.getMessage)
    val viaRestart = Condition.run[Int, Pure]((_, _) => Invoke("default", ()))(
      within[Int, Pure]("default")(raiseC[HowManyCase, Int](HowManyCase("retries")))(_ => 7))
    assertEquals(!.run(viaRestart), 7)
  }

  test("Resume continues AT the signal point: progress before it survives") {
    var steps = Vector.empty[String]
    val prog: Int ! C =
      for {
        _ <- pure[C, Unit] { steps :+= "before"; () }
        v <- signal[Int]("how many?")
        _ <- pure[C, Unit] { steps :+= s"after($v)"; () }
      } yield v + 1
    assertEquals(!.run(Condition.run[Int, Pure]((_, _) => Resume(41))(prog)), 42)
    assertEquals(steps, Vector("before", "after(41)"))
  }

  test("Invoke unwinds exactly to the named frame; outside continues, between never resumes") {
    var trail = Vector.empty[String]
    val prog: String ! C =
      for {
        a <- within[String, Pure]("use-default") {
          for {
            _ <- pure[C, Unit] { trail :+= "inside"; () }
            v <- signal[String]("bad value")
            _ <- pure[C, Unit] { trail :+= "never"; () }
          } yield v
        }(v => s"default:$v")
        _ <- pure[C, Unit] { trail :+= "outside"; () }
      } yield a
    val out = !.run(Condition.run[String, Pure] { (_, menu) =>
      assertEquals(menu, Vector("use-default"))
      Invoke("use-default", "42")
    }(prog))
    assertEquals(out, "default:42")
    assertEquals(trail, Vector("inside", "outside"))
  }

  test("the menu accumulates inner-first; invoking the OUTER restart unwinds past the inner") {
    var menus = Vector.empty[Vector[String]]
    var innerRecovered = false
    val prog: String ! C =
      within[String, Pure]("outer") {
        within[String, Pure]("inner")(signal[String]("deep")) { v => innerRecovered = true; s"inner:$v" }
      }(v => s"outer:$v")
    val out = !.run(Condition.run[String, Pure] { (_, menu) => menus :+= menu; Invoke("outer", "x") }(prog))
    assertEquals(out, "outer:x")
    assertEquals(menus, Vector(Vector("inner", "outer")))
    assert(!innerRecovered, "the inner frame recovered on the way past")
  }

  test("Fail escalates as Unhandled, naming the condition and the declined menu") {
    val prog: Int ! C = within[Int, Pure]("skip")(signal[Int]("broken"))(_ => 0)
    val e = intercept[Unhandled](!.run(Condition.run[Int, Pure]((_, _) => Fail)(prog)))
    assertEquals(e.condition, "broken")
    assertEquals(e.menu, Vector("skip"))
  }

  test("invoking a restart that is not on the menu is the policy's bug, named") {
    val e = intercept[NoSuchRestart](!.run(Condition.run[Int, Pure]((_, _) => Invoke("elsewhere", ()))(signal[Int]("x"))))
    assertEquals(e.restart, "elsewhere")
  }

  test("a frame whose body completes normally is invisible") {
    var recovered = false
    val out = !.run(Condition.run[Int, Pure]((_, _) => Fail)(within[Int, Pure]("unused")(pure(7)) { _ => recovered = true; 0 }))
    assertEquals(out, 7)
    assert(!recovered)
  }

  test("other effects forward: signal-and-resume inside a Writer row") {
    type R = Condition + Writer[String]
    val prog: Int ! R =
      for {
        _ <- Writer.tell("a").plus[Condition]
        b <- signal[Int]("double it").plus[Writer[String]]
        _ <- Writer.tell(s"b=$b").plus[Condition]
      } yield b + 2
    val (told, out) = !.run(Writer.run[String, Int, Pure](Condition.run[Int, Writer[String]]((_, _) => Resume(40))(prog)))
    assertEquals(out, 42)
    assertEquals(told.toList, List("a", "b=40"))
  }

  test("frames nested a hundred thousand deep by a recursive program: no host frame each") {
    def nest(n: Int): Int ! C =
      if (n == 0) signal[Int]("bottom")
      else within[Int, Pure](s"level")(!.tailcall(nest(n - 1)).map(_ + 1))(_ => -1)
    assertEquals(!.run(Condition.run[Int, Pure]((_, _) => Resume(0))(nest(100000))), 100000)
  }

  test("the repair story: one decode loop, three outcomes, chosen at run") {
    def decode(raw: String): Int ! C = raw.toIntOption match {
      case Some(n) => pure(n)
      case None => signal[Int](Damaged(raw))
    }
    def loop(raws: List[String]): Vector[Int] ! C = raws match {
      case Nil => pure(Vector.empty)
      case r :: rest =>
        for {
          head <- within[Option[Int], Pure]("skip")(decode(r).map(Some(_)))(_ => None)
          more <- loop(rest)
        } yield head.fold(more)(_ +: more)
    }
    val input = List("1", "x", "3")
    val patched = !.run(Condition.run[Vector[Int], Pure] {
      case (Damaged(_), _) => Resume(2)
      case _ => Fail
    }(loop(input)))
    assertEquals(patched, Vector(1, 2, 3))
    val skipped = !.run(Condition.run[Vector[Int], Pure] {
      case (Damaged(_), _) => Invoke("skip", ())
      case _ => Fail
    }(loop(input)))
    assertEquals(skipped, Vector(1, 3))
    val e = intercept[Unhandled](!.run(Condition.run[Vector[Int], Pure]((_, _) => Fail)(loop(input))))
    assertEquals(e.condition, Damaged("x"))
  }

  // typed conditions (the core's TestConditionTyped, its direct-block test aside)
  object HowManyOf extends Of[Int]
  object WhichName extends Of[String]

  test("a typed condition round-trips through the typed resume") {
    val prog: Int ! C = for { n <- HowManyOf.signal; s <- WhichName.signal } yield n + s.length
    val out = !.run(Condition.run[Int, Pure] {
      case (HowManyOf, _) => resume(HowManyOf)(40)
      case (WhichName, _) => resume(WhichName)("ab")
      case (_, _) => Fail
    }(prog))
    assertEquals(out, 42)
  }

  test("a wrong-typed resume is a compile error at the policy") {
    val e = compileErrors("""okay2.Condition.resume(okay2.TestConditionTypedFixture.HowManyOf)("not an int")""")
    assert(e.nonEmpty)
    assert(e.contains("Int") || e.contains("String"), e)
  }

  test("Of IS an Answers instance: raiseC takes it without a given, and a bad resume is named") {
    val viaRaise: Int ! C = raiseC(HowManyOf).map(_ + 1)
    assertEquals(!.run(Condition.run[Int, Pure] { case (HowManyOf, _) => Resume(41); case _ => Fail }(viaRaise)), 42)
    val bad = intercept[BadResume](!.run(Condition.run[Int, Pure]((_, _) => Resume("not an int"))(HowManyOf.signal)))
    assert(bad.getMessage.contains("not an int"), bad.getMessage)
  }
}

/** top-level homes: a case class nested in the suite cannot be type-tested (its outer reference), and the
 * compile-error test needs a stable path */
object TestConditionTypedFixture {
  final case class HowMany(what: String)
  final case class Damaged(raw: String)
  object HowManyOf extends Condition.Of[Int]
}
