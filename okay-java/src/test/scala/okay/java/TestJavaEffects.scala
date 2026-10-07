package okay.java

import okay.freer.{%, +, Pure}
import okay.freer.{!}
import okay.std.{Reader, State}
import okay.freer.Row.plus
import okay.freer.given
import okay.java.examples.JavaEffects
import okay.testkit.Munit.Diagnosed

/**
 * The Java facade (specs/java-effects.md), exercised through Java sources:
 * every program and handler here is written in `JavaEffects.java`, so a
 * signature that Java could not call, or could call only with casts, fails
 * to compile before any assertion runs.
 */
class TestJavaEffects extends munit.FunSuite, Diagnosed:

  test("form 1: a Java effect answered per operation"):
    assertEquals(JavaEffects.answered(), 42)

  test("form 2: a Java effect over a threaded state"):
    assertEquals(JavaEffects.counted(), Stated[Integer, Integer](12, 21))

  test("form 3: a Java effect translated into the core State"):
    assertEquals(JavaEffects.intoState(), Stated[Integer, Integer](2, 3))

  test("form 4: control resumes twice — every answer of two flips"):
    assertEquals(JavaEffects.allFlips(), java.util.List.of[Integer](3, 1, 2, 0))

  test("form 4: control never resumes — the rest is dropped"):
    assertEquals(JavaEffects.failing(false), java.util.Optional.of[Integer](2))
    assertEquals(JavaEffects.failing(true), java.util.Optional.empty[Integer]())

  test("an operation nothing handled is refused by name, not answered with itself"):
    val e = intercept[IllegalStateException](JavaEffects.unhandled())
    note(e.getMessage)
    assert(e.getMessage.contains("JavaEffects$Counter$Next"), e.getMessage)

  test("two Java effects, handled in either order"):
    assertEquals(JavaEffects.logInside(), "3 [got 1, got 2] 3")
    assertEquals(JavaEffects.logOutside(), "3 [got 1, got 2] 3")

  test("the core effects from Java: Reader, State, Throws"):
    assertEquals(JavaEffects.core(5), Stated[Integer, String](6, "s=6"))
    assertEquals(JavaEffects.core(200), Stated[Integer, String](201, "recovered too big: 201"))

  test("the core Async from Java"):
    assertEquals(JavaEffects.async(), 42)

  test("stack safety: 1 000 000 State steps by Java recursion, and a defer-ed tail call"):
    assertEquals(JavaEffects.deepState(1000000), 1000000)
    assertEquals(JavaEffects.deepDefer(1000000L), 500000500000L)

  test("a Scala program handled by Java handlers"):
    val p: Int ! (Reader % Int + State % Int) =
      Reader.ask[Int].plus[State % Int].flatMap(r => State.modify[Int](_ + r).plus[Reader % Int])
    val r = Eff.from(p).handle(Handler.reader(5)).handle(StateHandler.state(1)).run()
    assertEquals(r, Stated(6, 6))

  test("a Java program run by Scala handlers, a stranger refused by name"):
    val prog: Int ! Reader % Int = Eff.ask[Int]().map(_ + 1).toScala[Reader % Int]
    assertEquals(!.run(Reader.run[Int, Int, Pure](41)(prog)), 42)
    val stranger = JavaEffects.twoNexts().toScala[Reader % Int]
    val e = intercept[IllegalArgumentException](!.run(Reader.run[Int, Integer, Pure](41)(stranger)))
    note(e.getMessage)
    assert(e.getMessage.contains("JavaEffects$Counter$Next"), e.getMessage)
