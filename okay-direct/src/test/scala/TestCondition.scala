package okay


import Condition.*
import Condition.Decision.*

/** the condition battery (specs/condition.md): resume at the
 * point, unwind to the frame, the menu, escalation, forwarding —
 * and the repair story: one program, three outcomes, chosen at run */
class TestCondition extends munit.FunSuite {

  test("condition-caps: the lexical invoke unwinds to ITS frame; no handle, no invoke") {
    import Condition.*
    // the body holds its restart as a capability and leaves through
    // it directly — no signal, no policy round-trip
    val prog: String ! Op + Pure =
      frame[String, Int, Pure]("skip") { restart ?=>
        pure[Int, Op + Pure](1).flatMap { _ =>
          restart.invoke[String](42)   // abandon the rest of the frame
        }.map(_ => "never reached")
      }(v => s"skipped with $v")
    assertEquals((Condition.run(
      (_, _) => throw new AssertionError("the policy must not be consulted"))(prog)).run,
      "skipped with 42")
    // nesting: the inner handle leaves the inner frame; the outer
    // continues past it
    val nested: String ! Op + Pure =
      frame[String, String, Pure]("outer") { outer ?=>
        frame[String, Int, Pure]("inner") { inner ?=>
          inner.invoke[String](7)
        }(v => s"inner=$v").map(x => x + ", outer went on")
      }(v => s"outer=$v")
    assertEquals((Condition.run((_, _) =>
      Decision.Fail)(nested)).run, "inner=7, outer went on")
    // and the capability route crosses frames too: the OUTER handle
    // invoked from inside the inner frame skips both
    val crossing: String ! Op + Pure =
      frame[String, String, Pure]("outer") { outer ?=>
        frame[String, Int, Pure]("inner") { inner ?=>
          outer.invoke[String]("all the way out")
        }(v => s"inner=$v").map(x => x + ", never appended")
      }(v => s"outer=$v")
    assertEquals((Condition.run((_, _) =>
      Decision.Fail)(crossing)).run, "outer=all the way out")
    // no frame, no handle: a Restart cannot be conjured
    val errors = compileErrors("summon[okay.Condition.Restart[Int]]")
    assert(errors.nonEmpty, "a Restart outside every frame must not summon")
  }

  test("condition-caps: a handle targets ITS frame by identity — two frames of one name cannot alias") {
    import Condition.*
    // the outer "retry" recovers from an Int, the inner "retry" from a
    // String; the OUTER handle invoked inside the inner frame must
    // reach the outer recover — by name it would land in the inner
    // one and cast the Int to a String
    val aliased: String ! Op + Pure =
      frame[String, Int, Pure]("retry") { outer ?=>
        frame[String, String, Pure]("retry") { inner ?=>
          outer.invoke[String](3)
        }(s => s"inner got ${s.length}").map(x => x + ", never appended")
      }(n => s"outer got $n")
    assertEquals((Condition.run((_, _) =>
      Decision.Fail)(aliased)).run, "outer got 3")
    // the policy's Invoke stays BY NAME: innermost wins, as the menu promises
    val byName: String ! Op + Pure =
      frame[String, Int, Pure]("retry") { _ ?=>
        frame[String, String, Pure]("retry") { _ ?=>
          signal[String]("which?")
        }(s => s"inner got $s")
      }(n => s"outer got $n")
    assertEquals((Condition.run((_, _) =>
      Decision.Invoke("retry", "x"))(byName)).run, "inner got x")
  }


  test("the typed pair: raiseC answers its instance's type; a bad Resume is the policy's bug, named") {
    import Condition.*
    final case class HowMany(what: String)
    given Answers[HowMany, Int] = Answers.of[HowMany, Int]
    // the site needs NO annotation: the instance carries the type
    val prog: Int ! Op = raiseC(HowMany("retries")).map(_ * 2)
    assertEquals((Condition.run(
      (_, _) => Decision.Resume(21))(prog)).run, 42)
    // a policy resuming with the wrong type is refused NAMED at the
    // point where it acts — not a ClassCastException three calls later
    val bad = intercept[BadResume]((Condition.run(
      (_, _) => Decision.Resume("twenty-one"))(prog)).run)
    assert(bad.getMessage.contains("HowMany"), bad.getMessage)
    assert(bad.getMessage.contains("not a"), bad.getMessage)
    // restarts compose with the typed door exactly as with signal
    val viaRestart = Condition.run(
      (_, _) => Decision.Invoke("default", ()))(
      within[Int, Pure]("default")(
        raiseC(HowMany("retries")))(_ => 7))
    assertEquals(viaRestart.run, 7)
  }


  test("Resume continues AT the signal point: progress before it survives") {
    var steps = Vector.empty[String]
    val prog: Int ! Op =
      for
        _ <- pure[Unit, Op] { steps :+= "before"; () }
        v <- signal[Int]("how many?")
        _ <- pure[Unit, Op] { steps :+= s"after($v)"; () }
      yield v + 1
    val out = (Condition.run((_, _) => Resume(41))(prog)).run
    assertEquals(out, 42)
    assertEquals(steps, Vector("before", "after(41)"))
  }

  test("Invoke unwinds exactly to the named frame; outside continues, between never resumes") {
    var trail = Vector.empty[String]
    val prog: String ! Op =
      for
        a <- within[String, Pure]("use-default") {
          for
            _ <- pure[Unit, Op] { trail :+= "inside"; () }
            v <- signal[String]("bad value")
            _ <- pure[Unit, Op] { trail :+= "never"; () }
          yield v
        }(v => s"default:$v")
        _ <- pure[Unit, Op] { trail :+= "outside"; () }
      yield a
    val out = (Condition.run { (_, menu) =>
      assertEquals(menu, Vector("use-default"))
      Invoke("use-default", "42")
    }(prog)).run
    assertEquals(out, "default:42")
    assertEquals(trail, Vector("inside", "outside"))
  }

  test("the menu accumulates inner-first; invoking the OUTER restart unwinds past the inner") {
    var menus = Vector.empty[Vector[String]]
    var innerRecovered = false
    val prog: String ! Op =
      within[String, Pure]("outer") {
        within[String, Pure]("inner") {
          signal[String]("deep")
        } { v => innerRecovered = true; s"inner:$v" }
      }(v => s"outer:$v")
    val out = (Condition.run { (_, menu) =>
      menus :+= menu
      Invoke("outer", "x")
    }(prog)).run
    assertEquals(out, "outer:x")
    assertEquals(menus, Vector(Vector("inner", "outer")))
    assert(!innerRecovered, "the inner frame recovered on the way past")
  }

  test("Fail escalates as Unhandled, naming the condition and the declined menu") {
    val prog: Int ! Op = within[Int, Pure]("skip")(signal[Int]("broken"))(_ => 0)
    val e = intercept[Unhandled](
      (Condition.run((_, _) => Fail)(prog)).run)
    assertEquals(e.condition, "broken")
    assertEquals(e.menu, Vector("skip"))
  }

  test("invoking a restart that is not on the menu is the policy's bug, named") {
    val e = intercept[NoSuchRestart](
      (Condition.run((_, _) => Invoke("elsewhere", ()))(signal[Int]("x"))).run)
    assertEquals(e.restart, "elsewhere")
  }

  test("frames nested a hundred thousand deep by a recursive program: no host frame each, not quadratic") {
    // condition-nested-frames: a nested frame's region was entered by a direct call (a host frame each) and
    // every frame built the menu's names up front (O(depth) a frame)
    def nest(n: Int): Int ! Op =
      if n == 0 then signal[Int]("bottom")
      else within[Int, Pure]("level")(tailcall(nest(n - 1)).map(_ + 1))(_ => -1)
    assertEquals((Condition.run((_, _) => Resume(0))(nest(100000))).run, 100000)
  }

  test("a frame whose body completes normally is invisible") {
    var recovered = false
    val out = (Condition.run((_, _) => Fail)(
      within[Int, Pure]("unused")(pure[Int, Op + Pure](7)) { _ => recovered = true; 0 })).run
    assertEquals(out, 7)
    assert(!recovered)
  }

  test("other effects forward: signal-and-resume inside an Async row") {
    val prog: Int ! Op + Async =
      for
        a <- async(20).at[Op + Async]
        b <- signal[Int]("double it").at[Op + Async]
        c <- async(2).at[Op + Async]
      yield a + b + c
    val out = (Async.run(
      Condition.run((_, _) => Resume(20))(prog))).run
    assertEquals(out, 42)
  }

  /** records where "x" is damage; a class-level member (not local to
   * the test) so its type test in the policy matches below is
   * statically checkable — a local class cannot be, by JVM rule */
  private final case class Damaged(raw: String)

  test("the repair story: one decode loop, three outcomes, chosen at run") {
    def decode(raw: String): Int ! Op =
      raw.toIntOption match
        case Some(n) => pure(n)
        case None => signal[Int](Damaged(raw))

    // ONE frame per element: skipping abandons only that element's
    // decode, the tail always runs
    def loop(raws: List[String]): Vector[Int] ! Op =
      raws match
        case Nil => pure(Vector.empty)
        case r :: rest =>
          for
            head <- within[Option[Int], Pure]("skip")(decode(r).map(Some(_)))(_ => None)
            more <- loop(rest)
          yield head.fold(more)(_ +: more)

    val input = List("1", "x", "3")

    // policy 1: patch — the corrected value flows into the decode
    val patched = (Condition.run {
      case (Damaged(_), _) => Resume(2)
      case _ => Fail
    }(loop(input))).run
    assertEquals(patched, Vector(1, 2, 3))

    // policy 2: skip — the element vanishes, the loop continues
    val skipped = (Condition.run {
      case (Damaged(_), _) => Invoke("skip", ())
      case _ => Fail
    }(loop(input))).run
    assertEquals(skipped, Vector(1, 3))

    // policy 3: fail — the loop aborts, naming the damage
    val e = intercept[Unhandled]((Condition.run {
      (_, _) => Fail
    }(loop(input))).run)
    assertEquals(e.condition, Damaged("x"))
  }
}
