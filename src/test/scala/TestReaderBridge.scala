package okay

import okay.!.*

/**
 * specs/context-functions.md, ctx-reader-bridge: `Reader.lift` and
 * `Reader.unlift` — the round trips, and one Ask per lift.
 */
class TestReaderBridge extends munit.FunSuite:

  test("lift: a context function is a Reader program that asks once") {
    var applied = 0
    val cf: Int ?=> Int = { applied += 1; summon[Int] * 2 }
    val p: Int ! Reader % Int = Reader.lift(cf)
    assertEquals(applied, 0)                                   // nothing ran at lift
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](21)(p)), 42)
    assertEquals(applied, 1)
    // one Ask per lift: count the operations by stepping
    var asks = 0
    val counted = relay[Int, Int, Reader % Int, okay.Pure](p)(pure(_)):
      [X, Y] => e => e match
        case Reader.Ask() => asks += 1; Cont.Pure(5)
    assertEquals(!.run(counted), 10)
    assertEquals(asks, 1)
  }

  test("unlift: a Reader program is a context function run under the ambient E, the rest forwarded") {
    type R = Reader % Int + Writer % String
    val p: Int ! R =
      !.widen[Int, Reader % Int, Writer % String](Reader.ask[Int]).flatMap(e =>
        !.widen[Unit, Writer % String, Reader % Int](Writer.tell(s"env=$e")).map(_ => e + 1))
    val cf: Int ?=> Int ! Writer % String = Reader.unlift[Int, Int, Writer % String](p)
    val (log, a) = !.run(Writer.run[String, Int, okay.Pure](provide(41)(cf)))
    assertEquals((log, a), (Seq("env=41"), 42))
  }

  test("the round trips: unlift(lift(cf)) is cf; lift(unlift(p)) at e is p at e") {
    val cf: String ?=> Int = summon[String].length
    val back: String ?=> Int ! okay.Pure = Reader.unlift[String, Int, okay.Pure](Reader.lift(cf))
    assertEquals(!.run(provide("four")(back)), provide("four")(cf))
    val p: Int ! Reader % Int = Reader.ask[Int].map(_ * 3)
    val again: Int ! Reader % Int = Reader.lift(Reader.unlift[Int, Int, okay.Pure](p) match { case prog => !.run(prog) })
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](7)(again)), !.run(Reader.run[Int, Int, okay.Pure](7)(p)))
  }
