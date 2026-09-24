package okay

import okay.!.*
import okay.Row.{plus, bind, andThen}

/**
 * specs/writer-covariance.md, bind-in: a bind across rows, the other
 * row inferred — the same tree `flatMap` would build after two
 * `plus`es, with nothing named.
 */
class TestBind extends munit.FunSuite:

  type Two = Reader % Int + Writer % String

  test("the other row is read off the continuation; the same answer and log as the two-plus spelling") {
    val viaBindIn: Int ! Two = Reader.ask[Int].bind(e => Writer.tell(s"env=$e").map(_ => e + 1))
    val viaPlus: Int ! Two = Reader.ask[Int].plus[Writer % String].flatMap(e => Writer.tell(s"env=$e").map(_ => e + 1).plus[Reader % Int])
    def run(p: Int ! Two) = !.run(Writer.run[String, Int, okay.Pure](Reader.run[Int, Int, Writer % String](41)(p)))
    assertEquals(run(viaBindIn), (Seq("env=41"), 42))
    assertEquals(run(viaBindIn), run(viaPlus))
  }

  test("three rows, two binds: the union of all three, effects in order") {
    val p: Int ! Reader % Int + Writer % String + State % Int =
      Reader.ask[Int]
        .bind(e => Writer.tell(s"saw $e"))
        .bind(_ => State.modify[Int](_ + 1))
    val ((log, (cell, out))) = !.run(Writer.run[String, (Int, Int), okay.Pure](
      Reader.run[Int, (Int, Int), Writer % String](5)(State.handle(10)(p))))
    assertEquals(log, Seq("saw 5"))
    assertEquals((cell, out), (11, 11))
  }

  test("the tree is one Bind whose continuation is f — no walk, no re-injection") {
    var asked = 0
    val p: Int ! Two = Reader.ask[Int].bind(e => Writer.tell(s"$e").map(_ => e))
    // stepping: the head form is the Ask, and its continuation is the program f builds
    (p.resume: @unchecked) match
      case Bind(Inject(Reader.Ask()), _) => ()
      case other => fail(s"not a single Bind on the Ask: $other")
    val counted = relay[Int, Int, Reader % Int, Writer % String](p)(pure(_)):
      [X, Y] => e => e match
        case Reader.Ask() => asked += 1; Cont.Pure(3)
    assertEquals(!.run(Writer.run[String, Int, okay.Pure](counted)), (Seq("3"), 3))
    assertEquals(asked, 1)
  }

  test("andThen: both sides run, in order, the first answer dropped") {
    val p: Int ! Two = Reader.ask[Int].andThen(Writer.tell("after").map(_ => 7))
    assertEquals(!.run(Writer.run[String, Int, okay.Pure](Reader.run[Int, Int, Writer % String](1)(p))), (Seq("after"), 7))
  }
